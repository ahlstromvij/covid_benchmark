set.seed(100)

library(tidyverse)

# read in data
control <- read.csv("data/control.csv", na.strings=c(""," ","NA"))
treat1 <- read.csv("data/treat1.csv", na.strings=c(""," ","NA"))
treat2 <- read.csv("data/treat2.csv", na.strings=c(""," ","NA"))

# remove all disqualified or incomplete
table(control$Status)
control <- subset(control, Status=="Complete")
table(treat1$Status)
treat1 <- subset(treat1, Status=="Complete")
table(treat2$Status)
treat2 <- subset(treat2, Status=="Complete")

# rename columns
names(control)
control <- control %>% 
  rename(
    prolific_id = Please.enter.your.Prolific.ID.to.indicate.that.you.consent.to.taking.part.in.the.study.,
    age = What.is.your.age.,
    gender = What.gender.do.you.identify.with.,
    education = What.is.your.highest.level.of.education.,
    ethnicity = To.which.of.these.groups.do.you.consider.you.belong.,
    income = What.is.your.gross.household.income.annually..This.is.the.combined.income.of.all.earners.in.a.household.from.all.sources..including.wages..salaries..or.rents.and.before.tax.deductions.,
    partisanship = Which.party..if.any..do.you.identify.most.closely.with.,
    eu_ref = Thinking.back.to.the.EU.referendum.in.2016..did.you.vote.to.remain.in.the.EU.or.to.leave.the.EU..or.did.you.not.vote.,
    k_scale_virus = COVID.19.is.a.virus.Please.tick.all.statements.that.are.true.,
    k_scale_bacteria = COVID.19.is.caused.by.bacteria.Please.tick.all.statements.that.are.true.,
    k_scale_hot = COVID.19.can.be.transmitted.in.areas.with.hot.and.humid.climate.Please.tick.all.statements.that.are.true.,
    k_scale_vaccine = There.is.no.coronavirus.vaccine.approved.for.use.in.the.general.population.Please.tick.all.statements.that.are.true.,
    k_scale_recover = Most.people.who.get.COVID.19.recover.from.it.Please.tick.all.statements.that.are.true.,
    k_scale_mosquito = COVID.19.can.be.transmitted.through.mosquito.bites.Please.tick.all.statements.that.are.true.,
    k_scale_antibiotics = Antibiotics.are.effective.in.preventing.and.treating.COVID.19.Please.tick.all.statements.that.are.true.,
    k_scale_none = None.of.these.are.true.Please.tick.all.statements.that.are.true.,
    contact_scale_tested = I.have.tested.positive.for.coronavirus.or.for.coronavirus.antibodies.Please.tick.all.that.apply.,
    contact_scale_confident = I.have.not.been.tested..but.am.highly.confident.that.I.have.or.have.had.coronavirus.Please.tick.all.that.apply.,
    contact_scale_know_tested = Someone.I.know.has.tested.positive.for.coronavirus.or.for.coronavirus.antibodies.Please.tick.all.that.apply.,
    contact_scale_know_passed = Someone.I.know.has.passed.away.from.a.confirmed.case.of.coronavirus.Please.tick.all.that.apply.,
    contact_scale_treating = My.job.involves.treating.coronavirus.patients..or.supporting.those.who.are.giving.treatment.Please.tick.all.that.apply.,
    contact_scale_none = None.of.these.apply.Please.tick.all.that.apply.,
    numeracy_fractions = How.good.are.you.at.working.with.fractions.,
    numeracy_discount = How.good.are.you.at.figuring.out.how.much.a.shirt.will.cost.if.it.is.25..off.,
    numeracy_useful = How.often.do.you.find.numerical.information.to.be.useful.,
    trust_understands = The.government.understands.the.needs.of.my.community.Do.you.agree.or.disagree.with.the.following.statements.,
    trust_intentions = The.government.usually.has.good.intentions.Do.you.agree.or.disagree.with.the.following.statements.,
    trust_does_right = In.general..the.government.usually.does.the.right.thing.Do.you.agree.or.disagree.with.the.following.statements.,
    distrust_no_respect = Politicians.don.t.respect.people.like.me.Do.you.agree.or.disagree.with.the.following.statements.,
    distrust_ignore = Politicians.usually.ignore.my.community.Do.you.agree.or.disagree.with.the.following.statements.,
    distrust_unfair = The.government.acts.unfairly.towards.people.like.me.Do.you.agree.or.disagree.with.the.following.statements.,
    worry_scale_contracting = Contracting.COVID.19..the.disease.caused.by.coronavirus..Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_unwell = Becoming.seriously.unwell.or.dying.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_fr_fam = Friends.or.family.becoming.seriously.unwell.or.dying.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_finances = Your.finances.being.severely.affected.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_job = Losing.your.job.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_society = There.being.a.long.lasting.negative.impact.on.society.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    govt_perform_comm = I.have.found.the.communication.and.advice.from.the.UK.government.helpful.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_advice = The.UK.government.s.advice.on.how.to.protect.myself.and.others.has.been.effective.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_response = The.UK.government.s.response.to.the.coronavirus.has.been.clear.and.consistent.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_adapt = The.UK.government.s.plan.has.adapted.well.to.the.changing.scientific.information.and.situation.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_compared = Compared.with.other.countries..the.UK.government.has.responded.well.to.the.coronavirus.outbreak.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_protecting = The.UK.government.has.done.a.good.job.of.protecting.UK.residents.through.its.response.to.the.coronavirus.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    restrictions_quarantine = Quarantining.anyone.who.has.been.in.contact.with.a.contaminated.patient.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_schools = Temporarily.closing.schools.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_events = Cancelling.large.events.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_hospital = Cancelling.routine.hospital.procedures.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_stop_flights = Stopping.all.inbound.international.flights.from.countries.with.confirmed.cases.of.coronavirus.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_quar_flights = Quarantining.all.inbound.international.flights.from.countries.with.confirmed.cases.of.coronavirus.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_soc_dist = Encouraging.social.distancing.of.at.least.1.metre.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_mask_transport = Requiring.wearing.masks.on.public.transport.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_mask_indoors = Requiring.wearing.masks.in.all.indoor.public.places.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_none = I.support.none.of.these.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    behavior_hairdresser = Going.to.the.hairdressers.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_museum = Visiting.an.in.door.museum.or.exhibition.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_bars = Going.to.bars.and.restaurants.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_public_toilet = Using.public.toilets.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_public_transport = Using.public.transport.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_cinemas = Going.to.in.door.cinemas.or.theatres.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_abroad = Taking.holidays.abroad.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_worship = Attending.places.of.worship.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_gym = Going.to.in.door.gyms...leisure.centres...swimming.pools.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_public_gathering = Going.to.large..public.gatherings..sport...music.events..In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_ERROR = Use.public.transport..e.g...bus..train..Underground.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_none = None.of.these.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    prolific = Prolific
  )

treat1 <- treat1 %>% 
  rename(
    prolific_id = Please.enter.your.Prolific.ID.to.indicate.that.you.consent.to.taking.part.in.the.study.,
    age = What.is.your.age.,
    gender = What.gender.do.you.identify.with.,
    education = What.is.your.highest.level.of.education.,
    ethnicity = To.which.of.these.groups.do.you.consider.you.belong.,
    income = What.is.your.gross.household.income.annually..This.is.the.combined.income.of.all.earners.in.a.household.from.all.sources..including.wages..salaries..or.rents.and.before.tax.deductions.,
    partisanship = Which.party..if.any..do.you.identify.most.closely.with.,
    eu_ref = Thinking.back.to.the.EU.referendum.in.2016..did.you.vote.to.remain.in.the.EU.or.to.leave.the.EU..or.did.you.not.vote.,
    k_scale_virus = COVID.19.is.a.virus.Please.tick.all.statements.that.are.true.,
    k_scale_bacteria = COVID.19.is.caused.by.bacteria.Please.tick.all.statements.that.are.true.,
    k_scale_hot = COVID.19.can.be.transmitted.in.areas.with.hot.and.humid.climate.Please.tick.all.statements.that.are.true.,
    k_scale_vaccine = There.is.no.coronavirus.vaccine.approved.for.use.in.the.general.population.Please.tick.all.statements.that.are.true.,
    k_scale_recover = Most.people.who.get.COVID.19.recover.from.it.Please.tick.all.statements.that.are.true.,
    k_scale_mosquito = COVID.19.can.be.transmitted.through.mosquito.bites.Please.tick.all.statements.that.are.true.,
    k_scale_antibiotics = Antibiotics.are.effective.in.preventing.and.treating.COVID.19.Please.tick.all.statements.that.are.true.,
    k_scale_none = None.of.these.are.true.Please.tick.all.statements.that.are.true.,
    contact_scale_tested = I.have.tested.positive.for.coronavirus.or.for.coronavirus.antibodies.Please.tick.all.that.apply.,
    contact_scale_confident = I.have.not.been.tested..but.am.highly.confident.that.I.have.or.have.had.coronavirus.Please.tick.all.that.apply.,
    contact_scale_know_tested = Someone.I.know.has.tested.positive.for.coronavirus.or.for.coronavirus.antibodies.Please.tick.all.that.apply.,
    contact_scale_know_passed = Someone.I.know.has.passed.away.from.a.confirmed.case.of.coronavirus.Please.tick.all.that.apply.,
    contact_scale_treating = My.job.involves.treating.coronavirus.patients..or.supporting.those.who.are.giving.treatment.Please.tick.all.that.apply.,
    contact_scale_none = None.of.these.apply.Please.tick.all.that.apply.,
    numeracy_fractions = How.good.are.you.at.working.with.fractions.,
    numeracy_discount = How.good.are.you.at.figuring.out.how.much.a.shirt.will.cost.if.it.is.25..off.,
    numeracy_useful = How.often.do.you.find.numerical.information.to.be.useful.,
    trust_understands = The.government.understands.the.needs.of.my.community.Do.you.agree.or.disagree.with.the.following.statements.,
    trust_intentions = The.government.usually.has.good.intentions.Do.you.agree.or.disagree.with.the.following.statements.,
    trust_does_right = In.general..the.government.usually.does.the.right.thing.Do.you.agree.or.disagree.with.the.following.statements.,
    distrust_no_respect = Politicians.don.t.respect.people.like.me.Do.you.agree.or.disagree.with.the.following.statements.,
    distrust_ignore = Politicians.usually.ignore.my.community.Do.you.agree.or.disagree.with.the.following.statements.,
    distrust_unfair = The.government.acts.unfairly.towards.people.like.me.Do.you.agree.or.disagree.with.the.following.statements.,
    worry_scale_contracting = Contracting.COVID.19..the.disease.caused.by.coronavirus..Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_unwell = Becoming.seriously.unwell.or.dying.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_fr_fam = Friends.or.family.becoming.seriously.unwell.or.dying.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_finances = Your.finances.being.severely.affected.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_job = Losing.your.job.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_society = There.being.a.long.lasting.negative.impact.on.society.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    govt_perform_comm = I.have.found.the.communication.and.advice.from.the.UK.government.helpful.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_advice = The.UK.government.s.advice.on.how.to.protect.myself.and.others.has.been.effective.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_response = The.UK.government.s.response.to.the.coronavirus.has.been.clear.and.consistent.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_adapt = The.UK.government.s.plan.has.adapted.well.to.the.changing.scientific.information.and.situation.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_compared = Compared.with.other.countries..the.UK.government.has.responded.well.to.the.coronavirus.outbreak.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_protecting = The.UK.government.has.done.a.good.job.of.protecting.UK.residents.through.its.response.to.the.coronavirus.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    restrictions_quarantine = Quarantining.anyone.who.has.been.in.contact.with.a.contaminated.patient.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_schools = Temporarily.closing.schools.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_events = Cancelling.large.events.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_hospital = Cancelling.routine.hospital.procedures.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_stop_flights = Stopping.all.inbound.international.flights.from.countries.with.confirmed.cases.of.coronavirus.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_quar_flights = Quarantining.all.inbound.international.flights.from.countries.with.confirmed.cases.of.coronavirus.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_soc_dist = Encouraging.social.distancing.of.at.least.1.metre.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_mask_transport = Requiring.wearing.masks.on.public.transport.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_mask_indoors = Requiring.wearing.masks.in.all.indoor.public.places.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_none = I.support.none.of.these.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    behavior_hairdresser = Going.to.the.hairdressers.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_museum = Visiting.an.in.door.museum.or.exhibition.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_bars = Going.to.bars.and.restaurants.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_public_toilet = Using.public.toilets.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_public_transport = Using.public.transport.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_cinemas = Going.to.in.door.cinemas.or.theatres.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_abroad = Taking.holidays.abroad.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_worship = Attending.places.of.worship.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_gym = Going.to.in.door.gyms...leisure.centres...swimming.pools.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_public_gathering = Going.to.large..public.gatherings..sport...music.events..In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_ERROR = Use.public.transport..e.g...bus..train..Underground.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_none = None.of.these.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    prolific = Prolific,
    page_timer = Picture.page.timer,
    time_on_page = Time.spent.on.picture.page,
    manip_check = Thinking.back.to.the.picture.you.saw.earlier..how.many.countries.appeared.in.it.
  )

treat2 <- treat2 %>% 
  rename(
    prolific_id = Please.enter.your.Prolific.ID.to.indicate.that.you.consent.to.taking.part.in.the.study.,
    age = What.is.your.age.,
    gender = What.gender.do.you.identify.with.,
    education = What.is.your.highest.level.of.education.,
    ethnicity = To.which.of.these.groups.do.you.consider.you.belong.,
    income = What.is.your.gross.household.income.annually..This.is.the.combined.income.of.all.earners.in.a.household.from.all.sources..including.wages..salaries..or.rents.and.before.tax.deductions.,
    partisanship = Which.party..if.any..do.you.identify.most.closely.with.,
    eu_ref = Thinking.back.to.the.EU.referendum.in.2016..did.you.vote.to.remain.in.the.EU.or.to.leave.the.EU..or.did.you.not.vote.,
    k_scale_virus = COVID.19.is.a.virus.Please.tick.all.statements.that.are.true.,
    k_scale_bacteria = COVID.19.is.caused.by.bacteria.Please.tick.all.statements.that.are.true.,
    k_scale_hot = COVID.19.can.be.transmitted.in.areas.with.hot.and.humid.climate.Please.tick.all.statements.that.are.true.,
    k_scale_vaccine = There.is.no.coronavirus.vaccine.approved.for.use.in.the.general.population.Please.tick.all.statements.that.are.true.,
    k_scale_recover = Most.people.who.get.COVID.19.recover.from.it.Please.tick.all.statements.that.are.true.,
    k_scale_mosquito = COVID.19.can.be.transmitted.through.mosquito.bites.Please.tick.all.statements.that.are.true.,
    k_scale_antibiotics = Antibiotics.are.effective.in.preventing.and.treating.COVID.19.Please.tick.all.statements.that.are.true.,
    k_scale_none = None.of.these.are.true.Please.tick.all.statements.that.are.true.,
    contact_scale_tested = I.have.tested.positive.for.coronavirus.or.for.coronavirus.antibodies.Please.tick.all.that.apply.,
    contact_scale_confident = I.have.not.been.tested..but.am.highly.confident.that.I.have.or.have.had.coronavirus.Please.tick.all.that.apply.,
    contact_scale_know_tested = Someone.I.know.has.tested.positive.for.coronavirus.or.for.coronavirus.antibodies.Please.tick.all.that.apply.,
    contact_scale_know_passed = Someone.I.know.has.passed.away.from.a.confirmed.case.of.coronavirus.Please.tick.all.that.apply.,
    contact_scale_treating = My.job.involves.treating.coronavirus.patients..or.supporting.those.who.are.giving.treatment.Please.tick.all.that.apply.,
    contact_scale_none = None.of.these.apply.Please.tick.all.that.apply.,
    numeracy_fractions = How.good.are.you.at.working.with.fractions.,
    numeracy_discount = How.good.are.you.at.figuring.out.how.much.a.shirt.will.cost.if.it.is.25..off.,
    numeracy_useful = How.often.do.you.find.numerical.information.to.be.useful.,
    trust_understands = The.government.understands.the.needs.of.my.community.Do.you.agree.or.disagree.with.the.following.statements.,
    trust_intentions = The.government.usually.has.good.intentions.Do.you.agree.or.disagree.with.the.following.statements.,
    trust_does_right = In.general..the.government.usually.does.the.right.thing.Do.you.agree.or.disagree.with.the.following.statements.,
    distrust_no_respect = Politicians.don.t.respect.people.like.me.Do.you.agree.or.disagree.with.the.following.statements.,
    distrust_ignore = Politicians.usually.ignore.my.community.Do.you.agree.or.disagree.with.the.following.statements.,
    distrust_unfair = The.government.acts.unfairly.towards.people.like.me.Do.you.agree.or.disagree.with.the.following.statements.,
    worry_scale_contracting = Contracting.COVID.19..the.disease.caused.by.coronavirus..Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_unwell = Becoming.seriously.unwell.or.dying.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_fr_fam = Friends.or.family.becoming.seriously.unwell.or.dying.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_finances = Your.finances.being.severely.affected.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_job = Losing.your.job.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    worry_scale_society = There.being.a.long.lasting.negative.impact.on.society.Thinking.about.the.possible.impacts.of.coronavirus..how.worried.are.you.about.the.following.,
    govt_perform_comm = I.have.found.the.communication.and.advice.from.the.UK.government.helpful.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_advice = The.UK.government.s.advice.on.how.to.protect.myself.and.others.has.been.effective.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_response = The.UK.government.s.response.to.the.coronavirus.has.been.clear.and.consistent.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_adapt = The.UK.government.s.plan.has.adapted.well.to.the.changing.scientific.information.and.situation.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_compared = Compared.with.other.countries..the.UK.government.has.responded.well.to.the.coronavirus.outbreak.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    govt_perform_protecting = The.UK.government.has.done.a.good.job.of.protecting.UK.residents.through.its.response.to.the.coronavirus.To.what.extent..if.at.all..do.you.agree.or.disagree.with.the.following.statements.,
    restrictions_quarantine = Quarantining.anyone.who.has.been.in.contact.with.a.contaminated.patient.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_schools = Temporarily.closing.schools.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_events = Cancelling.large.events.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_hospital = Cancelling.routine.hospital.procedures.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_stop_flights = Stopping.all.inbound.international.flights.from.countries.with.confirmed.cases.of.coronavirus.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_quar_flights = Quarantining.all.inbound.international.flights.from.countries.with.confirmed.cases.of.coronavirus.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_soc_dist = Encouraging.social.distancing.of.at.least.1.metre.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_mask_transport = Requiring.wearing.masks.on.public.transport.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_mask_indoors = Requiring.wearing.masks.in.all.indoor.public.places.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    restrictions_none = I.support.none.of.these.Societies.can.respond.in.a.number.of.ways.to.coronavirus..Which.of.the.following.actions.would.you.support..Tick.all.that.apply.,
    behavior_hairdresser = Going.to.the.hairdressers.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_museum = Visiting.an.in.door.museum.or.exhibition.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_bars = Going.to.bars.and.restaurants.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_public_toilet = Using.public.toilets.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_public_transport = Using.public.transport.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_cinemas = Going.to.in.door.cinemas.or.theatres.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_abroad = Taking.holidays.abroad.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_worship = Attending.places.of.worship.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_gym = Going.to.in.door.gyms...leisure.centres...swimming.pools.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_public_gathering = Going.to.large..public.gatherings..sport...music.events..In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_ERROR = Use.public.transport..e.g...bus..train..Underground.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    behavior_none = None.of.these.In.the.next.month..which.of.the.following.activities.are.you.likely.to.do..Tick.all.that.apply.,
    prolific = Prolific,
    page_timer = Picture.page.timer,
    time_on_page = Time.spent.on.picture.page,
    manip_check = Thinking.back.to.the.picture.you.saw.earlier..how.many.countries.appeared.in.it.
  )

control$condition <- "control"
treat1$condition <- "treat1"
treat2$condition <- "treat2"

# check manipulation
table(treat1$manip_check) # correct: one
# treat1 <- subset(treat1, treat1$manip_check=="One")
table(treat2$manip_check) # correct: more than one
# treat2 <- subset(treat2, treat2$manip_check=="More than one")

manipulation_df <- data.frame(
  manip = rep(c("One", "More than one"), 2),
  condition = c("UK-Only", "UK-Only", "Comparative", "Comparative"),
  part = c(921, 83, 9, 1000),
  y = c(921/2+83, 83/2, 1000+9+30, 1000/2)
)

manipulation_df <- manipulation_df %>%
  arrange(manip) %>%
  mutate(manip = factor(manip, levels=c("One","More than one")))

ggplot(manipulation_df, aes(x = condition, y = part))+
  geom_col(aes(fill = manip), width = 0.7) +
  ylab("Number of participants") +
  xlab("Condition") +
  theme(legend.title = element_blank()) +
  geom_text(aes(y = y, label = part, group = manip), color = c("white","white","white","black"))

control <- add_column(control, page_timer = NA, .after = "distrust_unfair")
control <- add_column(control, time_on_page = NA, .after = "page_timer")
control <- add_column(control, manip_check = NA, .after = "behavior_none")

all_data <- rbind(control,treat1,treat2)
all_data$prolific <- NULL
all_data$page_timer <- NULL
all_data$behavior_ERROR <- NULL # double-check that empty for all

# remove duplicates
duplicates <- all_data %>% 
  group_by(prolific_id) %>% 
  filter(n()>1)
all_data <- subset(all_data, prolific_id!="5e62d0f8142c3c06a33fcba4" & 
                     prolific_id!="5eff1a979aca1b4554ab9d94" &
                     prolific_id!="5edb8780b940fe8107b640d1")

# check if anyone less than 5 sec on time on page
summary(all_data$time_on_page) # no

# read in prolific csv:s to look at country of residence
prolific1 <- read.csv("data/prolific/1.csv")
prolific2 <- read.csv("data/prolific/2.csv")
prolific3 <- read.csv("data/prolific/3.csv")
prolific4 <- read.csv("data/prolific/4.csv")
prolific5 <- read.csv("data/prolific/5.csv")
prolific6 <- read.csv("data/prolific/6.csv")
prolific7 <- read.csv("data/prolific/7.csv")
prolific8 <- read.csv("data/prolific/8.csv")
prolific9 <- read.csv("data/prolific/9.csv")
prolific10 <- read.csv("data/prolific/10.csv")
prolific11 <- read.csv("data/prolific/11.csv")
prolific12 <- read.csv("data/prolific/12.csv")
prolific13 <- read.csv("data/prolific/13.csv")

prolific <- rbind(prolific1,
                  prolific2,
                  prolific3,
                  prolific4,
                  prolific5,
                  prolific6,
                  prolific7,
                  prolific8,
                  prolific9,
                  prolific10,
                  prolific11,
                  prolific12,
                  prolific13)
table(prolific$Current.Country.of.Residence)

all_data$residence <- NA
all_data$nationality <- NA
all_data$residence <- prolific$Current.Country.of.Residence[match(all_data$prolific_id,prolific$participant_id)]
all_data$nationality <- prolific$Nationality[match(all_data$prolific_id,prolific$participant_id)]
table(all_data$nationality)
table(all_data$residence)

# recode gender (as given by prolific) where given as other, if too small number
table(all_data$gender)
all_data$gender[all_data$gender=="Other"] <- prolific$Sex[match(all_data$prolific_id[all_data$gender=="Other"],prolific$participant_id)]

# simplified ethnicity variable
all_data$ethnicity_simple <- NA
all_data$ethnicity_simple <- ifelse(all_data$ethnicity=="White", "White", "Other")
table(all_data$ethnicity_simple)

# income variable
table(all_data$income)
all_data$income <- as.character(all_data$income)
all_data$income[all_data$income=="Under £10,000 per year"] <- 1
all_data$income[all_data$income=="£10,001 - 20,000 per year"] <- 2
all_data$income[all_data$income=="£20,001 - 30,000 per year"] <- 3
all_data$income[all_data$income=="£30,001 - 40,000 per year"] <- 4
all_data$income[all_data$income=="£40,001 - 50,000 per year"] <- 5
all_data$income[all_data$income=="£50,001 - 60,000 per year"] <- 6
all_data$income[all_data$income=="£60,001 - 70,000 per year"] <- 7
all_data$income[all_data$income=="£70,001 - 80,000 per year"] <- 8
all_data$income[all_data$income=="£80,001 - 90,000 per year"] <- 9
all_data$income[all_data$income=="£90,001 - 100,000 per year"] <- 10
all_data$income[all_data$income=="£100,001 - 150,000 per year"] <- 11
all_data$income[all_data$income=="Over 150,000 per year"] <- 12

# age variable
table(all_data$age)
all_data$age[all_data$age=="fifty nine"] <- 59
all_data$age[all_data$age=="I 46"] <- 46
all_data <- all_data[!(all_data$age=="5d11c32dadb1c800010ea6b8"),]
all_data <- all_data[!(all_data$age=="Alex"),]
all_data <- all_data[!(all_data$age=="361"),]
class(all_data$age)
all_data$age <- as.numeric(as.character(all_data$age))
sum(!is.na(all_data$age))
class(all_data$age)
summary(all_data$age)
table(all_data$age)

# recruitment targets 
prop.table(table(all_data$gender[all_data$condition=="control"],all_data$ethnicity_simple[all_data$condition=="control"]))
prop.table(table(all_data$gender[all_data$condition=="treat1"],all_data$ethnicity_simple[all_data$condition=="treat1"]))
prop.table(table(all_data$gender[all_data$condition=="treat2"],all_data$ethnicity_simple[all_data$condition=="treat2"]))

# randomization check
tapply(all_data$age, all_data$condition, mean)
tapply(as.numeric(all_data$income), all_data$condition, mean)
prop.table(table(all_data$gender,all_data$condition), margin = 2)
prop.table(table(all_data$education,all_data$condition), margin = 2)
prop.table(table(all_data$ethnicity_simple,all_data$condition), margin = 2)
prop.table(table(all_data$partisanship,all_data$condition), margin = 2)
prop.table(table(all_data$eu_ref,all_data$condition), margin = 2)

table(all_data$condition)

saveRDS(all_data, "data/preprocessed_no_drop.rds")
