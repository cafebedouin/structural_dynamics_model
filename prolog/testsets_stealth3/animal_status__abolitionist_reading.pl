% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Instrumental Animal Use Regime - Abolitionist Reading (All Use as Rights Violation)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the contested
 *   animal_status kernel: sentient animals are rights-holders with inherent
 *   value, every instrumental use violates them, and welfare regulation
 *   functions as legitimation rather than protection. The epsilon referent is
 *   the standing arrangement under contest - the global regime of legally
 *   sanctioned instrumental animal use (roughly eighty billion land animals
 *   slaughtered annually, tens of millions in laboratories) - assessed by
 *   this reading's own lights, never the rights-respecting arrangement the
 *   reading endorses. Per the epsilon-invariance principle the rival readings
 *   are separate files linked by network edges:
 *   animal_status__welfare_reading authors moderate extraction over the same
 *   referent (suffering counts, use permitted);
 *   animal_status__property_reading authors near-zero extraction (animals as
 *   legal objects cannot be wronged). This file authors 0.96. The claim
 *   (snare) and the metrics are independently authored facts: the reading
 *   holds the arrangement's coordination story (feeding people, curing
 *   disease) is severable from the violation, and the metrics describe
 *   operation as the reading observes it - rising scale, expanding
 *   legitimation activity, hardening enforcement.
 *
 * KEY AGENTS:
 *   - - farmed_animals: Primary target (powerless/trapped) - bears total appropriation; nothing they generate is retained by them
 *   - - laboratory_animals: Secondary target (powerless/trapped) - bred as standardized instruments; scheduled endpoints
 *   - - animal_agribusiness: Primary beneficiary and informal co-agenda-setter (institutional/arbitrage) - collects revenue and funds the legislative defense of the boundary
 *   - - biomedical_research_establishment: Secondary beneficiary (institutional/identity_locked) - careers, curricula, and funding fused to the animal-model methodology
 *   - - meat_consuming_public: Diffuse beneficiary (moderate/constrained) - cheap habitual products; pays indirectly in health, environment, and taxes
 *   - - legislative_regulatory_apparatus: Agenda setter (institutional/mobile) - writes and enforces the boundary; capture-exposed via revolving door and campaign finance
 *   - - animal_rights_advocacy_movements: Excluded opposition (organized/trapped) - ag-gag statutes and SLAPP suits bar their evidence from the rooms where rules are set
 *   - - public_health_and_environmental_bodies: Analytical observer (institutional/analytical) - tracks zoonotic, antimicrobial-resistance, and emissions externalities without decision authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.96).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.85).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.96).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Instrumental Animal Use Regime - Abolitionist Reading (All Use as Rights Violation)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '036e88dd-8245-49e6-a958-4cfa225a5953').
narrative_ontology:cs_kernel_codification('036e88dd-8245-49e6-a958-4cfa225a5953', fixed_text).
narrative_ontology:cs_authority_grounding('036e88dd-8245-49e6-a958-4cfa225a5953', extraction).
narrative_ontology:cs_interpretation_layer_present('036e88dd-8245-49e6-a958-4cfa225a5953').
narrative_ontology:cs_reading_relation('036e88dd-8245-49e6-a958-4cfa225a5953', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('036e88dd-8245-49e6-a958-4cfa225a5953', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('036e88dd-8245-49e6-a958-4cfa225a5953', foundational, animals_possess_inherent_value_with_equal_consideration).
narrative_ontology:cs_axiom_status(animals_possess_inherent_value_with_equal_consideration, holdable).
narrative_ontology:cs_axiom_grounding('036e88dd-8245-49e6-a958-4cfa225a5953', animals_possess_inherent_value_with_equal_consideration, deontological).
narrative_ontology:cs_axiom('036e88dd-8245-49e6-a958-4cfa225a5953', foundational, instrumental_use_categorically_impermissible).
narrative_ontology:cs_axiom_status(instrumental_use_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('036e88dd-8245-49e6-a958-4cfa225a5953', instrumental_use_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('036e88dd-8245-49e6-a958-4cfa225a5953', secondary, animal_use_is_unnecessary_for_human_flourishing).
narrative_ontology:cs_axiom_status(animal_use_is_unnecessary_for_human_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('036e88dd-8245-49e6-a958-4cfa225a5953', animal_use_is_unnecessary_for_human_flourishing, empirically_contingent).
narrative_ontology:cs_reference_frame('036e88dd-8245-49e6-a958-4cfa225a5953', animals_as_rights_holders_baseline).
narrative_ontology:cs_drift_state('036e88dd-8245-49e6-a958-4cfa225a5953', contemporary_sentience_recognition_era, gap(revival_pressure, severe, false)).
narrative_ontology:cs_created_at('036e88dd-8245-49e6-a958-4cfa225a5953', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_agribusiness).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_research_establishment).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, meat_consuming_public).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, legislative_regulatory_apparatus).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, meat_consuming_public).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, animals_as_property_doctrine).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, welfare_regulation_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, necessary_use_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Roughly eighty billion land animals yearly are bred, housed, transported, and slaughtered according to production schedules set entirely by others. Feed, breeding, movement, social contact, and lighting are administered for output. They cannot buy, vote, sue, or flee; resistance is limited to struggle inside handling equipment. Everything their bodies produce leaves them.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Tens of millions are purpose-bred for toxicity testing, disease modeling, and basic research. Protocol documents written before their births determine housing, procedures, and endpoints; euthanasia is commonly the scheduled terminus. Replacement paperwork is filed in some jurisdictions while the breeding colonies continue operating.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Firms convert subsidized feed grain into animal products sold worldwide, collecting the revenue stream the arrangement generates. Trade associations lobby for subsidy renewal, sponsor model ag-gag legislation, fund university agriculture programs, and negotiate inspection standards. Capital exit is open: several of the largest firms have acquired plant-based and cultivated-protein subsidiaries.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agribusiness, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, animal_agribusiness, agenda_setter).

% Universities and contract research organizations receive grants and publish papers premised on standardized animal models, and their own review committees approve continued use. Curricula, tenure criteria, journal expectations, and facility investments assume the methodology continues, so switching methods threatens accumulated institutional identity and sunk training rather than presenting a neutral substitution decision.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_establishment, beneficiary,
    institutional, generational, identity_locked, continental).

% Consumers purchase inexpensive, habitual, culturally embedded animal products several times daily. They gain price and convenience; they pay indirectly through diet-related chronic disease, environmental degradation, and the tax share of subsidies and bailouts. Individual dietary change is feasible; collective change is rare.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, meat_consuming_public, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, meat_consuming_public, payer).

% Legislatures and agencies define which treatments count as lawful, set subsidy and inspection policy, and conduct the enforcement actions that keep operations proceeding. Many members move between agency posts and industry employment, and campaigns are financed substantially by the regulated sectors.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legislative_regulatory_apparatus, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, legislative_regulatory_apparatus, beneficiary).

% Organizations document conditions, litigate standing questions, and run ballot initiatives. Undercover recording, their principal evidence channel, is criminalized in a growing number of jurisdictions; lawsuits over investigative tactics consume budgets; committee access is minimal relative to industry representation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_rights_advocacy_movements, excluded,
    organized, biographical, trapped, continental).

% Agencies and scientific bodies monitor zoonotic spillover risk, antimicrobial resistance, waterway impacts, and greenhouse emissions attributable to animal agriculture. They publish risk assessments and recommendations but hold no vote on production or research decisions.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, public_health_and_environmental_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, animal_agribusiness).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates human demand for animal products and experimental subjects into standardized industrial systems: breeding lines, feed supply chains, slaughter and processing logistics, cold storage, and food-safety regimes; in research, standardized organisms make results comparable across laboratories. The reading disputes the necessity of the inputs, not the fact of the coordination.
% TRANSFER_FUNCTION: Moves animals' bodies, reproductive capacity, labor, and lives to human institutions: flesh, milk, eggs, and experimental data flow outward as firm revenue, grant output, and consumer goods, while mortality, confinement, suffering, zoonotic risk, and emissions remain with the animals and diffuse third parties.
% ABSENT_VOICES: The animals themselves hold no procedural standing anywhere in the system; no seat speaks in their voice except by proxy. Advocates attempting proxy voice face ag-gag prosecution and SLAPP exposure. Populations bearing environmental and pandemic externalities, and future generations inheriting both, hold no seat in use decisions.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, food systems, farm economies, research pipelines, subsidy architectures, and trade flows would all reorganize: livelihoods tied to livestock would need relocation, drug-development timelines would shift onto non-animal methods, and global crop allocation would swing from feed toward direct human consumption. Nothing physical or biological requires the arrangement; everything about its scale means removal rearranges the world.
% FOUNDING_PROBLEM: Securing reliable calories and protein before refrigeration, global grain trade, and synthetic nutrition existed; and, later, obtaining living test systems before cell culture, organoids, organs-on-chips, and computational modeling matured.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: dietary-adequacy literature and major dietetic associations affirm complete nutrition without animal products; the U.S. FDA Modernization Act 2.0 removed the mandatory animal-study requirement for investigational drugs; EU Directive 2010/63 mandates replacement wherever possible; OECD test-guideline programs increasingly admit validated non-animal methods. Industry bodies and portions of the toxicology establishment dispute full obsolescence, citing remaining technical gaps; that dispute is recorded here rather than resolved.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.96, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.96 because the arrangement appropriates animals' bodies, liberty, and lives in full and returns nothing they retain; the small deduction from unity reflects marginal goods some individuals receive (stunning mandates, enrichment requirements) that even this reading counts as received, however inadequate. Suppression (0.85) is overwhelmingly structural: property status denies standing, confinement and slaughter are the enforcement itself, and investigation is criminalized (ag-gag statutes, post-AETA terrorism enhancements). Theater ratio (0.66, rising from 0.30) tracks audit, labeling, and certification activity expanding faster than measurable improvement in per-animal condition - the reading's legitimation thesis operationalized; the crossing above 0.5 marks proxy-goal displacement. Accessibility collapse is LOW (0.30): plant-based protein at scale, precision fermentation, and validated non-animal methods mean alternatives do not disappear on understanding - persistence is maintained by subsidy architecture, habit, and statute rather than absence of alternatives. Resistance (0.58) reflects an organized, litigating, ballot-active movement plus the animals' own ineffective struggle. All three tracked metrics run on ONE shared grid (1975-2025, six points each). The suppression_requirement series is authored deliberately because this story specifically traces enforcement-capacity buildup - securitization of activism, processing-line-speed increases, expansion of the audit apparatus - not merely shifting extraction; a static picture would misstate the dynamic. Trajectories are monotonic rather than cyclical; no intermittent-reinforcement mechanism is posited.
 *
 * PERSPECTIVAL GAP:
 *   From the farmed_animals and laboratory_animals seats the arrangement computes as maximal appropriation with no exit whatsoever. From the agribusiness seat it is ordinary commerce whose rules the seat helps write - directionality near the beneficiary end, low effective extraction. From the consuming public seat it is background normality with diffuse indirect costs. From the research establishment seat, criticism registers as persecution of medicine rather than feedback about victims: identity fusion converts structural information into status threat, which is why that seat's exit is identity_locked despite institutional power. The engine computes these divergent per-seat classifications from the declared power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality downward: animal_agribusiness (arbitrage-grade capital mobility) sits nearest the beneficiary end; meat_consuming_public sits mid-low (incidentals gains against diffuse indirect payment); biomedical_research_establishment sits low but its identity_locked exit keeps it away from the arbitrage end despite institutional power. Victim declarations drive directionality toward the full-target end: both animal seats are powerless and trapped, so effective extraction amplifies to the maximum with no exit damping. The legislative_regulatory_apparatus carries a beneficiary secondary role (capture), pulling its derived directionality below that of a neutral administrator. No directionality_overrides were authored: roles, exits, and power atoms are cleanly differentiated in the structural data, so the derivation chain produces the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - pre-refrigeration protein security and pre-non-animal-method biomedical testing - is dead by this reading's lights: adequate nutrition and validated replacements exist, yet the arrangement persists at rising scale. Founding_problem_status=dead combined with disappearance_verdict=world_rearranges emits the persistence-past-function signature for the mismatch consumer to cross-check against the computed theater path. Classification from this seat prevents two mislabels: accepting the coordination story would read the arrangement as benign coordination and miss that its function is severable from the violation; reading it as inertial residue would miss the concentrated beneficiaries actively defending it. The residual uncertainty - whether any survival-necessity component remains in food-insecure regions or niche research applications - is carried by the necessity_obsolescence omega rather than baked into the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is the abolitionist instantiation the correct reading of the animal_status kernel, or do the welfare or property sibling readings correctly locate animals relative to the victim set?',
    'Philosophical adjudication of moral-status criteria (sentience, inherent value, relational accounts) together with doctrinal development; the sibling stories animal_status__welfare_reading and animal_status__property_reading carry the rival instantiations with their own epsilon values over the same referent.',
    'If the welfare reading prevails, the victim set shrinks to interest-violating cases, epsilon drops toward moderate levels, and the arrangement recomputes nearer a hybrid coordination shape; if the property reading prevails, animals exit the victim set entirely and measured extraction collapses toward zero. The structural delta between readings is located entirely in the victim-set boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer omega: this story is one reading (abolitionist) of kernel animal_status; sibling readings redraw the victim set and therefore the classification.').

omega_variable(
    welfare_legitimation_direction,
    'Do welfare regulations reduce net animal suffering and total use, or do they legitimate and entrench use while conditions stagnate?',
    'Comparative analysis across jurisdictions differing in welfare stringency (EU, US states, jurisdictions with no statutes): trajectories of per-animal condition, total slaughter volume, and consumption following reform waves.',
    'If reforms materially cut suffering and volume, the theater ratio falls and the arrangement softens toward a hybrid coordination/extraction profile; if legitimation dominates, theater keeps rising and the extraction verdict hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_legitimation_direction, empirical, 'Whether the expanding welfare apparatus is protective or performative.').

omega_variable(
    necessity_obsolescence,
    'Is instrumental animal use now technically avoidable at civilizational scale, or does a survival-necessity component remain?',
    'Nutritional adequacy literature, precision-fermentation and cultivated-protein cost curves, and validation rates for non-animal methods following the U.S. FDA Modernization Act 2.0 and EU Directive 2010/63 replacement mandates.',
    'If avoidable, the dead founding problem stands and the persistence-past-function signature is confirmed; if not, founding-problem status flips to contested and part of measured extraction is survival cost rather than discretionary appropriation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_obsolescence, empirical, 'Whether the arrangement''s original justification has a live residue.').

omega_variable(
    suppression_mechanism_composition,
    'How much of the measured suppression is structural (property status, confinement, criminalized investigation) versus internalized (speciesist attitudes that would survive barrier removal)?',
    'Post-legal-change attitude tracking in jurisdictions that extended sentience recognition: if pro-use attitudes persist after formal barriers fall, the internalized component is confirmed.',
    'If largely internalized, effective suppression exceeds the structural measure and persists through legal reform, requiring cultural rather than statutory remedy; if structural, statute-level change collapses suppression quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized split of the suppression carrying the arrangement.').

omega_variable(
    transition_weight_allocation,
    'What weight do present human transition costs (livelihoods, consumer prices, research disruption) carry against continued appropriation during any phase-out?',
    'Irreducible value weighting; resolvable only by political choice among compensation schedules, sunset timelines, and immediate-cessation proposals.',
    'High present-cost weighting favors managed-transition framings with sunset clauses; low weighting supports the categorical prohibition verdict and immediate cessation demands. This weighting, not evidence, fixes the residual classification fork.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_weight_allocation, preference, 'Value allocation between present human adjustment costs and ongoing animal-side costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1975, animal_status__abolitionist_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement_basis(anim_tr_t1975, observed).
narrative_ontology:measurement(anim_tr_t1985, animal_status__abolitionist_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement_basis(anim_tr_t1985, observed).
narrative_ontology:measurement(anim_tr_t1995, animal_status__abolitionist_reading, theater_ratio, 1995, 0.47).
narrative_ontology:measurement_basis(anim_tr_t1995, observed).
narrative_ontology:measurement(anim_tr_t2005, animal_status__abolitionist_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement_basis(anim_tr_t2005, observed).
narrative_ontology:measurement(anim_tr_t2015, animal_status__abolitionist_reading, theater_ratio, 2015, 0.62).
narrative_ontology:measurement_basis(anim_tr_t2015, observed).
narrative_ontology:measurement(anim_tr_t2025, animal_status__abolitionist_reading, theater_ratio, 2025, 0.66).
narrative_ontology:measurement_basis(anim_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t1975, animal_status__abolitionist_reading, base_extractiveness, 1975, 0.86).
narrative_ontology:measurement_basis(anim_be_t1975, observed).
narrative_ontology:measurement(anim_be_t1985, animal_status__abolitionist_reading, base_extractiveness, 1985, 0.89).
narrative_ontology:measurement_basis(anim_be_t1985, observed).
narrative_ontology:measurement(anim_be_t1995, animal_status__abolitionist_reading, base_extractiveness, 1995, 0.91).
narrative_ontology:measurement_basis(anim_be_t1995, observed).
narrative_ontology:measurement(anim_be_t2005, animal_status__abolitionist_reading, base_extractiveness, 2005, 0.93).
narrative_ontology:measurement_basis(anim_be_t2005, observed).
narrative_ontology:measurement(anim_be_t2015, animal_status__abolitionist_reading, base_extractiveness, 2015, 0.95).
narrative_ontology:measurement_basis(anim_be_t2015, observed).
narrative_ontology:measurement(anim_be_t2025, animal_status__abolitionist_reading, base_extractiveness, 2025, 0.96).
narrative_ontology:measurement_basis(anim_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1975, animal_status__abolitionist_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement_basis(anim_su_t1975, observed).
narrative_ontology:measurement(anim_su_t1985, animal_status__abolitionist_reading, suppression_requirement, 1985, 0.59).
narrative_ontology:measurement_basis(anim_su_t1985, observed).
narrative_ontology:measurement(anim_su_t1995, animal_status__abolitionist_reading, suppression_requirement, 1995, 0.63).
narrative_ontology:measurement_basis(anim_su_t1995, observed).
narrative_ontology:measurement(anim_su_t2005, animal_status__abolitionist_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement_basis(anim_su_t2005, observed).
narrative_ontology:measurement(anim_su_t2015, animal_status__abolitionist_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement_basis(anim_su_t2015, observed).
narrative_ontology:measurement(anim_su_t2025, animal_status__abolitionist_reading, suppression_requirement, 2025, 0.85).
narrative_ontology:measurement_basis(anim_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'animal status' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one referent (the standing instrumental-use arrangement) and diverging only in reading-indexed epsilon: the property reading authors near-zero extraction (objects cannot be wronged), the welfare reading authors moderate extraction (suffering counts, use permitted), and this abolitionist reading authors 0.96 (a rights violation per act of use, welfare reform counted as legitimation). The upstream sibling (property_reading) supplies the doctrinal foundation the other two contest; the welfare reading mediates between them and is the target of this reading's strongest structural criticism (that its reforms legitimate what they claim to limit). All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
