% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe-Memory Ritual as Survival-Competence Training Regime (Operational-Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   Across diaspora communities that remember repeated catastrophe, a yearly
 *   round of embodied observances — the spring remembrance meal with its
 *   hurried bread and scripted questions, midsummer fast days marking
 *   destroyed sanctuaries, retold deliverance stories — is maintained long
 *   after the events it marks have passed from living memory. This story
 *   instantiates the operational-competence reading of that arrangement: the
 *   observances function as a distributed training regime. Each iteration
 *   rehearses pattern recognition (reading early signs of danger in familiar
 *   narratives), resource coordination (rationing, hosting, and mutual-aid
 *   obligations exercised under scarcity rules), and threat assessment (what
 *   to take, whom to warn, how quickly to move). The arrangement's declared
 *   beneficiary is the community's future survival capacity; its recurring
 *   costs fall on current members' time, comfort, and attention, with a
 *   sharper edge for members who perform the forms as pure commemoration and
 *   thereby bank a settled feeling of having done what preparation requires.
 *   Transmission is embodied rather than documented: competence rides in
 *   practiced bodies and coordinated households, not in filed manuals. KEY
 *   AGENTS (by structural relationship): - ritual_calendar_authorities:
 *   Agenda-setter (institutional/constrained) — fixes the calendar,
 *   prescribes forms, adjudicates exemptions; collects administrative
 *   standing from the role - practicing_households: Primary beneficiary
 *   (moderate/identity_locked) — keeps the yearly round; rehearsed habits
 *   accumulate; exit equals leaving the family table -
 *   peripheral_participants: Marginal payer (moderate/constrained) — attends
 *   the visible markers, bears scheduling and expectation costs, receives
 *   diluted content - future_generations_of_the_community: Prospective
 *   beneficiary (powerless/trapped) — inherits the transmitted package
 *   without a seat in deciding it - symbol_only_observers: Principal payer
 *   (moderate/constrained) — performs as commemoration; obligations fully
 *   borne, practical habits unexercised - secular_preparedness_planners:
 *   Excluded professional counterpart (organized/mobile) — runs measured
 *   drill infrastructure; absent from the ritual conversation -
 *   ritual_studies_scholars: Analytical observer (moderate/analytical) —
 *   tests the transmission claim with comparative and quantitative methods -
 *   community_exiters: Excluded witness (moderate/arbitrage) — left the
 *   practice; attests to embedded habit and the persistence of obligation
 *
 * KEY AGENTS:
 *   - ritual_calendar_authorities: agenda-setting administrators whose standing rides on the calendar they keep
 *   - practicing_households: net beneficiaries whose rehearsed habits constitute the transmitted substance
 *   - peripheral_participants: cost-bearing attendees receiving diluted content
 *   - future_generations_of_the_community: voiceless inheritors of whatever package the present chooses to transmit
 *   - symbol_only_observers: full-cost performers who extract commemoration but not competence
 *   - secular_preparedness_planners: excluded operators of the functionally neighboring measured-drill infrastructure
 *   - ritual_studies_scholars: analytical observers of the transmission question
 *   - community_exiters: excluded witnesses to what the practice embeds and what leaving costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe-Memory Ritual as Survival-Competence Training Regime (Operational-Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, 'db9eafd2-d43e-4009-ac9b-b8d8a265a883').
narrative_ontology:cs_kernel_codification('db9eafd2-d43e-4009-ac9b-b8d8a265a883', distributed).
narrative_ontology:cs_authority_grounding('db9eafd2-d43e-4009-ac9b-b8d8a265a883', expertise).
narrative_ontology:cs_interpretation_layer_present('db9eafd2-d43e-4009-ac9b-b8d8a265a883').
narrative_ontology:cs_reading_relation('db9eafd2-d43e-4009-ac9b-b8d8a265a883', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('db9eafd2-d43e-4009-ac9b-b8d8a265a883', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('db9eafd2-d43e-4009-ac9b-b8d8a265a883', foundational, ritual_value_is_operational_yield).
narrative_ontology:cs_axiom_status(ritual_value_is_operational_yield, holdable).
narrative_ontology:cs_axiom_grounding('db9eafd2-d43e-4009-ac9b-b8d8a265a883', ritual_value_is_operational_yield, empirically_contingent).
narrative_ontology:cs_axiom('db9eafd2-d43e-4009-ac9b-b8d8a265a883', foundational, competence_decays_without_embodied_rehearsal).
narrative_ontology:cs_axiom_status(competence_decays_without_embodied_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('db9eafd2-d43e-4009-ac9b-b8d8a265a883', competence_decays_without_embodied_rehearsal, empirically_contingent).
narrative_ontology:cs_reference_frame('db9eafd2-d43e-4009-ac9b-b8d8a265a883', ritual_as_training_infrastructure).
narrative_ontology:cs_drift_state('db9eafd2-d43e-4009-ac9b-b8d8a265a883', contemporary_heritage_mode, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('db9eafd2-d43e-4009-ac9b-b8d8a265a883', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, practicing_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations_of_the_community).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, symbol_only_observers).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, peripheral_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, peripheral_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, direct_memory_two_generation_decay).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, costly_commitment_group_endurance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fix the festival calendar and prescribe the forms of observance: which nights the community gathers, what is eaten or refused, what is recounted, who is exempt for illness, age, or pregnancy. They adjudicate exemption requests and rule on novel circumstances — whether a fast may be broken for medical need, how the rites are kept under displacement or wartime. Their standing in the community rests on administering these duties well; stepping away would mean ceding the calendar to whoever picks it up. They describe the observances as commanded duties whose meaning includes readying the people for hardship.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_calendar_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Keep the observance year: host or attend the spring remembrance meal with its scripted questions and hurried-bread rules, keep the summer fasts marking the destroyed sanctuaries, retell the deliverance stories at the table. Through yearly repetition they keep alive habits of packing light, moving quickly, rationing, storing against interruption, and checking on neighbors. The costs are real — preparation labor, missed workdays, hungry days — and are paid as ordinary household expenditure. For the committed core, leaving the practice would feel like leaving the family itself; the practice and the household's sense of who they are have grown into one thing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, practicing_households, beneficiary,
    moderate, generational, identity_locked, global).

% Attend the main gatherings and keep the visible markers — the meal, a partial fast, the memorial candle — without organizing household life around the practice. What reaches them is belonging and story; the harder edges (full fast days, stocked pantries, drilled departures) mostly wash out. They carry the scheduling burden and the social expectation of attendance; opting out entirely would cost them kin ties and standing, staying costs them evenings they would spend otherwise.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, peripheral_participants, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__operational_competence_reading, peripheral_participants, beneficiary).

% Will inherit whichever version of the yearly round the current generation transmits — its habits along with its forms. They cannot decline the inheritance in advance and have no seat where the content of transmission is decided; what arrives is whatever the adults chose to keep, compressed into stories, foods, fasts, and reflexes. Whether the arriving package still carries usable readiness habits depends entirely on choices made now by people they cannot address.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_generations_of_the_community, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__operational_competence_reading, future_generations_of_the_community, excluded).

% Perform the observances as commemoration and heritage — they attend, fast, recount, and would say the point is memory and identity, full stop. The practical habits the performances were built to exercise stay unexercised: they do not pack lighter, store differently, or coordinate differently after the yearly round. They bear the full schedule of obligations and, beyond the schedule, a subtler cost — the settled feeling that having performed the rite, they have done what preparation requires.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbol_only_observers, payer,
    moderate, biographical, constrained, national).

% Build and run the functionally neighboring infrastructure: evacuation drills, stockpile guidance, mutual-aid registries, scenario exercises for agencies and employers. They measure retention and response times and treat unmeasured repetition with suspicion. They almost never appear in conversations about the ritual round's social value, and if seated they would press hard on whether unmeasured yearly rehearsal produces anything a stopwatch could confirm — or whether it quietly substitutes for measured preparation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, secular_preparedness_planners, excluded,
    organized, generational, mobile, national).

% Study the transmission question with the field's tools: comparative ethnography of disaster-commemorating communities, archival reconstruction of what the rites asked of bodies in harder centuries, quantitative work on why some congregations endure crisis better than others. They publish, dispute, and revise; nothing in the communities' practice waits on their verdict.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_studies_scholars, observer,
    moderate, generational, analytical, global).

% Have left the practicing world — stopped attending, married out, or simply drifted — and carry the residue: relatives who grieve the leaving, holidays that still arrive on the calendar, habits like checking the exits and keeping stores that outlasted belief. Several report the pull of obligation persisting years after any external pressure ended. They are the clearest living witnesses to what the practice embeds and what putting it down costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, community_exiters, excluded,
    moderate, biographical, arbitrage, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__operational_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__operational_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps survival-relevant competence alive across generations beyond the reach of living memory, and synchronizes the community on shared dates so readiness practice, scarcity discipline, and mutual-aid obligations are exercised together rather than by isolated households.
% TRANSFER_FUNCTION: Moves current members' time, attention, comfort, and calories (fast days, hosted gatherings, preparatory labor) into a stored communal repertoire — rehearsed reflexes, coordinated households, retold threat scripts — held available for members not yet born; a smaller stream moves administrative standing to the offices that keep the calendar.
% ABSENT_VOICES: Secular preparedness professionals, who operate the measured-drill counterpart and would demand outcome evidence the ritual frame never collected; community exiters, who know the exit price and the residue firsthand; future generations, who receive the package without a seat; and historians who read the rites as meaning made after the fact rather than training designed in advance.
% DISAPPEARANCE_RATIONALE: Without the yearly round, the rehearsed habits thin within a generation or two — the documented fate of readiness practice without recurrent exercise — and the community would have to invent deliberate replacement infrastructure (calendars of compulsory drills, taught curricula, audited supplies) to do what the shared calendar currently does as a side effect of observance; mourning and identity life would likewise rearrange around whatever new forms absorbed the dates.
% FOUNDING_PROBLEM: Repeated catastrophes — sanctuary destruction and expulsion in the ancient record; expulsions, massacres, and genocidal assault in the modern one — found communities whose members had grown up in safety; direct experience of what collapse demands faded within about two generations, and each new generation met the recurrence unready. The yearly round was built to compress catastrophe experience into repeatable annual rehearsal — hurried departure, scarce provisions, a watched horizon — so that readiness and mutual-aid coordination would outlive living memory.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: collective-memory research establishing the roughly two-generation horizon of direct experiential transmission; emergency-management literature on the decay of skills and plans absent recurrent rehearsal; and historical-demographic accounts of communities repeatedly caught unprepared a generation or two after each catastrophe. Communal authorities also attest the problem, but the sources named here stand outside the arrangement and its beneficiaries.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.28: the costs are real (preparation labor, hungry days, missed work, cognitive load) and fall on identifiable seats, but they are largely willingly borne and the delivered capacity accrues broadly to the same population and its successors — a reciprocal arrangement with a modest, slowly accumulating overhead as perceived benefit thins faster than cost decays. Suppression is authored low at 0.20: written protocols, instrumented drills, and secular preparedness infrastructure exist and are not barred by anything in this arrangement; what pressure remains is mostly social and internalized (see the suppression omega), not a load-bearing coercive wall. Accessibility_collapse at 0.30 reflects that understanding the mechanism does not eliminate substitutes — manuals and drills remain available — while the calendar-embedded, emotionally salient, body-level form gives the ritual vehicle durability advantages that paper substitutes have never matched at communal scale. Resistance at 0.22: exemption negotiation, selective observance, and quiet attrition occur; open revolt is rare because costs are tolerable and identity binds. Theater_ratio at 0.38 is the honest current share of heritage-mode performance versus training-mode exercise: the embodied substrate still exercises real habits even when framed as memory, but the fraction of observance that functions as pure commemorative performance has grown steadily across the interval.
 *   
 *   Temporal series run on ONE shared grid (every tracked metric authored at every point 0,10,...,80) telling one coherent story: enforcement capacity decayed (suppression_requirement 0.42 to 0.20 — the survivor generation's moral authority faded, formal discipline lapsed, exemption became routine), heritage-mode performance rose (theater_ratio 0.12 to 0.38), and the overhead crept up mildly (epsilon 0.20 to 0.28) as the benefit side thinned. Crisis episodes superimpose short spikes of functional engagement on this trend (each renewed catastrophe briefly re-functionalizes observance), but the secular drift dominates the interval and no full oscillation cycle is asserted in the series. The suppression_requirement series is authored precisely because this story traces enforcement-capacity change; the flat-scalar rule would have hidden the decay that drives the theater rise.
 *   
 *   Receipt-surface check performed seat by seat: costs dissipate into the rehearsal activity itself and the capacity accrues diffusely to households and their successors; no named seat pockets the arrangement's surplus — the authorities collect administrative standing from the role they occupy, not a share of the burden borne by payers — so gain_flow is affirmatively authored as diffuse. Fixing cost: while the founding problem is live, dismantling the yearly round without a tested replacement would forfeit the only transmission channel that demonstrably spans more than two living generations, so fixing_cost is prohibitive for every seat able to change it.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from identical structural data. From the practicing-household seat the yearly round prices as cheap recurring insurance whose premium is paid in time and appetite. From the symbol-only seat the same round prices as obligation without return — full cost, commemoration received, competence uncollected — and that seat's computed classification sits nearer the extractive end than any other payer's. From the authorities' seat the arrangement is sacred administration they are bound to keep; from the scholars' seat it is an open empirical bet whose payout is undetermined. Peripheral participants straddle: diluted content, real scheduling burden. The engine computes this divergence from role, power, and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: practicing_households and future_generations_of_the_community sit at the subsidized end (low d, damped or negative effective extraction) — the former actively collect rehearsed capacity, the latter passively receive it. Victim declarations drive the opposite pole: symbol_only_observers bear full obligations with the substance uncollected (high d, amplified extraction — and their identity-adjacent attachment to the forms keeps them from exercising the exit their dissatisfaction would otherwise motivate), and peripheral_participants sit mid-range with diluted benefit offsetting part of their cost. The authorities' d sits slightly above symmetric: they neither pay the bodily costs nor collect the competence, and their incidental administrative standing is modest. Exit modulation matters: identity_locked households sit nearer the beneficiary end than their cost share alone would predict (they would not exit even if indifferent), while mobile outsiders — the excluded planners — never enter the arrangement's cost structure at all. No directionality overrides were needed; the derivation chain from declared structure reproduces these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — catastrophes recur and experiential memory decays on a two-generation clock — so no mandatrophy is declared, and the rope classification is what prevents two symmetrical mislabels. Reading the arrangement as pure extraction (because payers exist and costs are visible) would erase the genuine coordination function that the beneficiary declarations and the vindicated decay findings establish; reading it as costless harmony (because beneficiaries dominate numerically) would whitewash the symbol-only seat, whose burden is real precisely because the transmitted substance is extractable and they do not extract it. The theater series tracks the degradation frontier: if the yield-verification omega returns null and theater_ratio continues its climb, the arrangement slides toward maintained performance with the function gone — the piton signature — while the live founding problem and the still-binding identity structure are what currently hold it on the rope side of that line.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the catastrophe_memory_transmission kernel; would the sibling readings (symbol_continuity_reading, hybrid_embedded_reading) classify the same yearly round differently?',
    'Generate the sibling stories against the same ritual corpus and compare computed types, epsilon, and victim sets. The disagreement locates in whether the transmitted good is separable operational substance, intrinsic symbolic form, or a fused embedding.',
    'Under symbol_continuity the victim class dissolves (no substance-extraction failure is possible) and epsilon falls toward the floor; under hybrid_embedded the yield-audit premise fails, element-by-element evaluation loses legitimacy, and assessment reverts to holistic fidelity of practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of classification within the kernel family.').

omega_variable(
    rope_or_mountain_status,
    'Is ritualized rehearsal an irreducible feature of how memory-bearing communities survive episodic catastrophe (a convergent cultural regularity wherever multi-generation catastrophe memory persists), or a contingent, replaceable coordination mechanism?',
    'Comparative anthropology across independent literate societies: if manual-and-drill cultures abandon ritual without measurable competence loss, the mechanism is replaceable (constructed side); if ritualized recurrence appears wherever catastrophe memory must span generations regardless of literacy or doctrine, the convergence supports a lawlike reading.',
    'A lawlike resolution would demand emerges_naturally=true, route naturality certification, and force re-examination of the declared beneficiaries for false-summit treatment; the constructed reading stands if substitution by documented protocols and drills succeeds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rope_or_mountain_status, conceptual, 'Whether the arrangement is constructed coordination or convergent cultural law.').

omega_variable(
    operational_yield_verification,
    'Does yearly embodied rehearsal actually confer measurable survival-relevant capacity — faster mobilization, better scarcity management, sounder threat judgments — or does the appearance of transmission rest on selection effects, with committed communities enduring for reasons unrelated to competence?',
    'Prospective comparison of matched rehearsing and non-rehearsing communities on outcomes independent of scheduled drills; archival natural experiments where observance lapsed or intensified for exogenous reasons; retention curves for rehearsed versus merely documented procedures.',
    'Confirmed yield secures this reading''s foundational axiom and the coordination classification; a null result collapses the axiom, leaving heritage performance as the residue and shrinking the victim class toward wasted effort alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_yield_verification, empirical, 'Empirical status of the competence-transmission claim itself.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the remaining pressure to keep the yearly round structural (communal sanction, marriage-market and standing effects, institutional expectation) or internalized (obligation-feeling and identity fusion that persist without external enforcement)?',
    'Post-exit trajectory study of community_exiters: if obligation-feeling and practice residues persist for years after all external pressure has ended, a substantial internalized share is indicated; if relief dominates the exit experience, the structural share dominates.',
    'Internalized pressure travels with the agent after exit, raising effective suppression above the structural measure and sharpening the identity-lock picture for practicing households.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of remaining enforcement pressure.').

omega_variable(
    false_confidence_magnitude,
    'How large is the harm borne by members who perform the rites as commemoration only — does ritual participation crowd out practical preparation, leave it unchanged, or complement it?',
    'Behavioral audits of readiness stocks (supplies, plans, physically rehearsed actions) among ritual-performing versus matched non-performing community members, controlling for income and risk exposure.',
    'Crowding-out would establish a material victim class and raise the effective burden attributable to the arrangement; neutrality would shrink the victim declaration toward the nominal; complementarity would support the coordination reading outright.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_confidence_magnitude, empirical, 'Size and reality of the false-assurance cost borne by symbol-only performers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_mem_op_comp_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cat_mem_op_comp_tr_t0, observed).
narrative_ontology:measurement(cat_mem_op_comp_tr_t10, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(cat_mem_op_comp_tr_t10, observed).
narrative_ontology:measurement(cat_mem_op_comp_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(cat_mem_op_comp_tr_t20, observed).
narrative_ontology:measurement(cat_mem_op_comp_tr_t30, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(cat_mem_op_comp_tr_t30, observed).
narrative_ontology:measurement(cat_mem_op_comp_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(cat_mem_op_comp_tr_t40, observed).
narrative_ontology:measurement(cat_mem_op_comp_tr_t50, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(cat_mem_op_comp_tr_t50, observed).
narrative_ontology:measurement(cat_mem_op_comp_tr_t60, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(cat_mem_op_comp_tr_t60, observed).
narrative_ontology:measurement(cat_mem_op_comp_tr_t70, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 70, 0.36).
narrative_ontology:measurement_basis(cat_mem_op_comp_tr_t70, observed).
narrative_ontology:measurement(cat_mem_op_comp_tr_t80, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(cat_mem_op_comp_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cat_mem_op_comp_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(cat_mem_op_comp_be_t0, observed).
narrative_ontology:measurement(cat_mem_op_comp_be_t10, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement_basis(cat_mem_op_comp_be_t10, observed).
narrative_ontology:measurement(cat_mem_op_comp_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement_basis(cat_mem_op_comp_be_t20, observed).
narrative_ontology:measurement(cat_mem_op_comp_be_t30, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement_basis(cat_mem_op_comp_be_t30, observed).
narrative_ontology:measurement(cat_mem_op_comp_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement_basis(cat_mem_op_comp_be_t40, observed).
narrative_ontology:measurement(cat_mem_op_comp_be_t50, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 50, 0.26).
narrative_ontology:measurement_basis(cat_mem_op_comp_be_t50, observed).
narrative_ontology:measurement(cat_mem_op_comp_be_t60, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 60, 0.27).
narrative_ontology:measurement_basis(cat_mem_op_comp_be_t60, observed).
narrative_ontology:measurement(cat_mem_op_comp_be_t70, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 70, 0.28).
narrative_ontology:measurement_basis(cat_mem_op_comp_be_t70, observed).
narrative_ontology:measurement(cat_mem_op_comp_be_t80, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement_basis(cat_mem_op_comp_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(cat_mem_op_comp_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(cat_mem_op_comp_su_t0, observed).
narrative_ontology:measurement(cat_mem_op_comp_su_t10, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(cat_mem_op_comp_su_t10, observed).
narrative_ontology:measurement(cat_mem_op_comp_su_t20, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement_basis(cat_mem_op_comp_su_t20, observed).
narrative_ontology:measurement(cat_mem_op_comp_su_t30, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement_basis(cat_mem_op_comp_su_t30, observed).
narrative_ontology:measurement(cat_mem_op_comp_su_t40, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 40, 0.27).
narrative_ontology:measurement_basis(cat_mem_op_comp_su_t40, observed).
narrative_ontology:measurement(cat_mem_op_comp_su_t50, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 50, 0.24).
narrative_ontology:measurement_basis(cat_mem_op_comp_su_t50, observed).
narrative_ontology:measurement(cat_mem_op_comp_su_t60, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(cat_mem_op_comp_su_t60, observed).
narrative_ontology:measurement(cat_mem_op_comp_su_t70, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 70, 0.21).
narrative_ontology:measurement_basis(cat_mem_op_comp_su_t70, observed).
narrative_ontology:measurement(cat_mem_op_comp_su_t80, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 80, 0.2).
narrative_ontology:measurement_basis(cat_mem_op_comp_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, resource_allocation).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% Constraint family per the epsilon-invariance principle: the colloquial label 'catastrophe-memory ritual' covers structurally distinct claims, decomposed into three stories. This file authors the operational-competence claim (epsilon 0.28, victim = symbol-only performers banking false assurance, yield-auditable). The symbol_continuity sibling authors the intrinsic-good claim (epsilon near the floor, no extraction asymmetry, no substance-failure victim possible). The hybrid_embedded sibling authors the inseparability claim (element-by-element yield auditing illegitimate; fidelity of fused practice is the unit of assessment). The three are rival readings of one kernel rather than causal dependencies, so coupling runs through cs_structure.reading_relations rather than resource-flow edges; each file links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
