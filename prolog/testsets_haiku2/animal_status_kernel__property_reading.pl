% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animal Status as Property (Property Reading of Animal Status Kernel)
 *   domain: moral_philosophy/legal_theory/animal_ethics
 *
 * SUMMARY:
 *   This constraint is one reading of the contested animal-status kernel:
 *   animals are property; their moral considerability derives entirely from
 *   ownership rights; economic value is the only normatively relevant value.
 *   The reading establishes that animals have no independent moral claims, no
 *   legal standing, and no interests that override owner prerogative or
 *   market allocation. The constraint manifests in property law (animals as
 *   chattels), commodity markets (pricing by productive capacity), research
 *   ethics (minimal animal-subject protections), and agricultural regulation
 *   (welfare constraints framed as protecting property value, not animal
 *   interests). The constraint is claimed as rope (coordination of
 *   allocation), but the authored metrics reflect substantially extractive
 *   operation with rising suppression requirement: extraction increases
 *   monotonically as productive capacity expands and welfare pressure mounts,
 *   forcing escalating enforcement to maintain the property frame against
 *   accumulating challenge.
 *
 * KEY AGENTS:
 *   - property_owners: institutional beneficiary (control, use rights, economic value)
 *   - animals: powerless payers (subject to unrestricted use, trapped, no institutional voice)
 *   - commodity_markets: institutional beneficiary (price discovery, production efficiency)
 *   - legal_institutions: agenda_setter (codify and enforce property status)
 *   - welfare_advocates: excluded but partly integrated (constrain use without challenging property)
 *   - research_institutions: beneficiary (unrestricted animal subjects)
 *   - observer_seat_moral_philosophers: analytical (measure structural divergence from sibling readings)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.89).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.76).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, rope).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Status as Property (Property Reading of Animal Status Kernel)").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/legal_theory/animal_ethics").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '0e608af2-b547-48da-8e4e-22b1fe4bc1a2').
narrative_ontology:cs_kernel_codification('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', fixed_text).
narrative_ontology:cs_authority_grounding('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', extraction).
narrative_ontology:cs_interpretation_layer_present('0e608af2-b547-48da-8e4e-22b1fe4bc1a2').
narrative_ontology:cs_reading_relation('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', foundational, animals_lack_independent_moral_status).
narrative_ontology:cs_axiom_status(animals_lack_independent_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', animals_lack_independent_moral_status, conventional).
narrative_ontology:cs_axiom('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', foundational, ownership_subsumes_all_moral_claims).
narrative_ontology:cs_axiom_status(ownership_subsumes_all_moral_claims, holdable).
narrative_ontology:cs_axiom_grounding('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', ownership_subsumes_all_moral_claims, deontological).
narrative_ontology:cs_axiom('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', secondary, economic_value_is_only_relevant_value).
narrative_ontology:cs_axiom_status(economic_value_is_only_relevant_value, holdable).
narrative_ontology:cs_axiom_grounding('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', economic_value_is_only_relevant_value, instrumental).
narrative_ontology:cs_reference_frame('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', property_rights_natural_baseline).
narrative_ontology:cs_drift_state('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', contemporary_animal_cognition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e608af2-b547-48da-8e4e-22b1fe4bc1a2', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, property_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, commodity_markets).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, research_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and control animals as property under law; derive economic value through use (food production, labor, research, entertainment, breeding). Set the normative frame: animals have no interests independent of owner preferences; welfare constraints are permitted only when protecting property value. Enforce this frame through legal institutions, market structures, and cultural transmission of ownership rights doctrine.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, property_owners, agenda_setter,
    powerful, generational, arbitrage, global).

% Subject to unrestricted use by owners within the bounds of property law. Bear the material costs of ownership (confinement, bodily alteration, forced reproduction, death for extraction). Have no legal standing to contest their status or conditions; no institutional mechanism to articulate interests; entirely dependent on owner discretion or anti-cruelty law (which protects property value, not animal interests per se).
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animals, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(animal_status_kernel__property_reading, animals).

% Would contest the property frame and argue animals have interests that override property rights. Excluded from the property-reading's core institutional logic: their objections are treated as external to the legitimate dispute (not a disagreement about what animals are, but external moral preference). They mount pressure via legislation, litigation, and cultural advocacy but operate within a framework that pre-positions their claims as sectarian rather than foundational.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, anti_cruelty_advocates, excluded,
    organized, biographical, constrained, global).

% Depend on animals-as-property and unrestricted use rights for valuation and pricing. Markets price animals by productive/consumptive capacity; the property frame enables the price discovery that drives production. Commodity futures, breeding indices, and supply-chain efficiency all presuppose animals have no independent claims that would constrain production.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, commodity_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Accept animals-as-property but argue use should be constrained by sentience and suffering. Within the property frame, their position is a minority constraint on owner prerogative; within their own frame, welfare constraints are insufficient because property status itself permits systematic harm. Excluded from the property reading's core claim but partly integrated into enforcement (welfare regulations shape use conditions without challenging ownership).
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, welfare_advocates, excluded,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, welfare_advocates, observer).

% Codify and enforce animals-as-property through tort law, contract law, and criminal law's anti-cruelty provisions. Treat animals as economic units whose damage or loss is compensable as property damage; exclude them from legal standing or rights-bearing status. Defend the property frame as natural law or rational baseline; treat alternatives as externally motivated expansions of moral concern.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legal_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Depend on animals-as-property to conduct research without animal subjects having legal standing to refuse or claim harm compensation. Use unrestricted because property status means no independently relevant interest is violated; research value is weighed only against owner/researcher interest, not against animal suffering. Minimal constraints from welfare law; maximum freedom in experimental design.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Analyze the property reading as one coherent position within a contested kernel. Do not hold this reading as normative claim but observe its structure, its internal consistency, its contradictions with sibling readings, and its empirical consequences for animal experience and institutional behavior.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, observer_seat_moral_philosophers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__property_reading, property_owners).
narrative_ontology:fixing_cost_class(animal_status_kernel__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal framework for allocating animals to productive and consumptive human uses: property law provides ownership, transfer, damage remedies, and exclusion rights without requiring negotiation of each animal's use conditions or interests. Solves the allocation problem by treating animals as economic inputs rather than negotiating parties.
% TRANSFER_FUNCTION: Animals, treated as productive or consumptive capital assets, transfer from production (breeding, capture, rearing) to consumption (slaughter, use, research, entertainment) under owner direction. Economic value flows to owners; the constraint ensures this flow is unencumbered by countervailing moral or legal claims from animals themselves.
% ABSENT_VOICES: Animals have no institutional voice in the property frame — they are excluded by definition because property status means their interests do not register as independent claims in law or ethics. Moral philosophers, animal advocates, and welfare practitioners would object but are positioned as external to the legitimate dispute over what animals are; their objections are treated as moral expansionism rather than accurate description.
% DISAPPEARANCE_RATIONALE: If the property reading disappeared and were replaced by one where animals had inalienable interests or moral status independent of ownership, the entire animal agriculture, biomedical research, and commodity complex would reorganize: use rights would be contested, production would require justification against animal claims, markets would price in constraint costs, many current practices would become illegal. The economic order presupposing animals-as-property would face structural collapse.
% FOUNDING_PROBLEM: How to allocate animals to human productive and consumptive purposes without infinite transaction costs or moral deadlock. Property law solves this: ownership vests all use decisions in the owner; animals' status as property ensures that allocation disputes are resolved by owner preference and market value, not by claims from the animals themselves.
% FOUNDING_PROBLEM_CORROBORATION: Property owners, commodity markets, research institutions, and legal scholars attest the founding problem is live and the property frame solves it efficiently. Welfare advocates and abolitionist philosophers attest the founding problem is misconstrued — it presupposes animals have no independent moral status and thus presents 'solving allocation' as the real problem, obscuring that the true problem is whether this allocation method is morally permissible at all. Outside the benefiting parties: animal cognition research (e.g., evidence of animal self-awareness, pain experience, social complexity) and comparative legal analysis of non-property readings suggest the founding problem statement is framed to exclude the very question (animal interests) that would dissolve it.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89 at interval end) because the constraint strips animals of all independent moral and legal claims; owners extract unrestricted use value subject only to market demand and minimal anti-cruelty law. The time series shows extraction rising from 0.78 to 0.89 over the interval: this reflects escalating production intensity, technological capacity to use animals more fully (genetic selection, confinement systems, reproductive control), and corresponding expansion of extraction despite welfare pressure — efficiency gains enlarge the use-value extracted per animal while suppression requirement rises to defend the property frame. Suppression rises from 0.65 to 0.76 because welfare advocacy, moral-philosophical challenge, and empirical evidence of animal cognition create mounting pressure to abandon the property reading; the constraint requires increasing active defense (enforcement of property status as natural/rational baseline, reframing of welfare advocacy as sectarian preference, institutional isolation of animal-cognition research from policy). Theater rises from 0.28 to 0.42: while the core use-extraction is real, an increasing share of institutional activity (welfare labeling, certification schemes, research ethics boards, anti-cruelty law) performs constraint defense and rhetorical legitimation rather than direct animal use. The constraint's justificatory layer thickens as its empirical vulnerability grows.
 *
 * PERSPECTIVAL GAP:
 *   Property owners compute this as rope (genuine coordination solving allocation problems without moral harm — animals have no independent moral status so no harm is done). Animals compute it as snare if they could (trapped by definition, no voice, extraction is total). Welfare advocates compute it as tangled_rope (coordination claim covers asymmetric extraction; property status itself is the enforcement mechanism). Abolitionist philosophers compute it as snare (the property frame is the cage; moral persons are treated as chattels). The engine computes per-seat from power/exit/beneficiary-victim declarations — the authored data produces this divergence structurally, without any seat's perception being adjudicated in advance.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners sit at d ≈ 0.0 (full beneficiary: they collect unrestricted use value, have high power and arbitrage exit). Animals sit at d ≈ 1.0 (full target: they bear material costs, are powerless and trapped, have no countervailing moral claim in this frame). Commodity markets sit at d ≈ 0.1 (beneficiary via efficient allocation). Research institutions sit at d ≈ 0.05 (beneficiary via animal subjects free from moral/legal constraint). Welfare advocates sit at d ≈ 0.55 (symmetric: they mount pressure but operate within an institutional logic that excludes their core premise; they are neither full targets nor beneficiaries but structurally constrained advocates). Legal institutions sit at d ≈ 0.1 (beneficiary as agenda-setter: they codify and defend the property frame, which is their institutional mandate; exit means institutional dissolution).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem ('how to allocate animals to human use without transaction costs') presupposes the property frame itself — it is a problem only if animals have no independent moral status. The contest over the animal-status kernel is precisely the contest over whether the founding problem is rightly stated. As welfare science and moral philosophy advance outside the property-owner seat, the founding problem dies: the real problem becomes 'how to weigh animal interests against human interests' — a problem the property frame was designed to foreclose by stipulating animals have no interests. The constraint exhibits mandatrophy: it persists despite mounting evidence that its founding problem is based on false premises (animals do have interests, do have cognitive complexity, do suffer). The constraint is maintained by enforcement escalation (suppression requirement rising) and by institutional isolation of evidence (research ethics restrictions, market-based welfare certification). A mandatrophy-resolved classification would treat this as a former rope whose coordination function has been superseded by empirical discovery, now sustained by pure power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_sentience_moral_relevance,
    'Is animal suffering (empirically demonstrated) morally relevant? Or does property status categorically exclude animal suffering from moral consideration?',
    'Empirical: neuroscience and ethology establish animal cognitive and affective capacities (pain systems, self-awareness, social emotion). Normative: philosophical analysis of whether ''property status'' is a coherent ground for excluding sentient beings from moral consideration, or whether sentience itself is the morally relevant fact.',
    'If suffering is morally relevant, the property reading must either abandon the claim or defend it on grounds other than animals lacking interests (e.g., owner property rights override animal interests). Reframing required; extraction classification may shift from rope to snare (hiding moral cost under property doctrine).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animal_sentience_moral_relevance, empirical, 'Whether animal sentience is a barrier to property status or irrelevant to it.').

omega_variable(
    property_as_natural_vs_constructed,
    'Is animals-as-property a natural fact (animals lack moral status in virtue of their nature) or a constructed legal/normative frame (animals are assigned property status as a matter of law, convention, or power)?',
    'Comparative legal/anthropological analysis: non-property frameworks for animals (some indigenous systems, some contemporary jurisdictions recognizing animals as legal persons). Historical analysis: property status is assigned by law, not discovered in nature. Philosophical analysis: the claim that animals ''naturally'' are property requires defending against the claim that animals naturally have interests.',
    'If property is constructed, the constraint is reclassified as a Snare: property status is the cage, not the neutral baseline. If natural, the property reading stands as rope (coordination framework). The false-summit detection system should flag this constraint as a FSM candidate if any animals-have-interests evidence accumulates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_as_natural_vs_constructed, conceptual, 'Whether property status is grounded in animal nature or in legal construction.').

omega_variable(
    suppression_mechanism_escalation,
    'Why does suppression_requirement rise over the interval? Is it response to external pressure (welfare advocacy, moral philosophy, empirical evidence) or internal institutional refinement?',
    'Trace enforcement escalation: new welfare regulations, research ethics restrictions, market-based certification schemes, institutional isolation of animal-cognition research. Correlate with advocacy activity, philosophical publication, empirical discovery.',
    'If suppression rises to defend property status against external challenge, the constraint''s persistence depends on escalating power/enforcement — classic Snare signature. If rising suppression reflects only internal efficiency gains or market refinement, the rope classification holds. The measurement series suggests response-to-pressure (theater ratio also rising), which indicates the property frame faces mounting structural challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_escalation, empirical, 'Whether suppression escalation reflects external pressure or internal institutional change.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the property reading logically foreclose the abolitionist reading within a single coherent framework, or can both coexist as live positions held by different parties?',
    'Logical analysis: if the property reading claims animals CAN BE property (a metaphysical claim about their nature), does this preclude the abolitionist claim that animals CANNOT BE property in the same logical space? Or are both claims about what law/morality SHOULD BE rather than about what IS?',
    'If foreclosure holds, the reading_relations entry for abolitionist_reading should be ''forecloses''. If coexistence holds, it should be ''coexists_with''. The distinction matters for computing kernel resolution: foreclosure implies one reading must be abandoned; coexistence implies institutional competition between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether the property and abolitionist readings foreclose each other logically or coexist as rival positions.').

omega_variable(
    welfare_integration_as_constraint_weakening,
    'Do welfare constraints (anti-cruelty law, welfare certification) weaken the property reading by acknowledging animal interests, or strengthen it by admitting regulation while retaining property status?',
    'Compare jurisdictions and time periods: do welfare constraints correlate with reduced acceptance of property status, or with stabilization of property status under augmented moral framing? Measure: does welfare law ever explicitly reject animals-as-property, or only constrain how property is used?',
    'If welfare integration weakens the property frame by legitimating animal interests, the constraint is under structural pressure and approaches mandatrophy. If welfare integration stabilizes it, the property frame adapts and persists. The theater_ratio rise suggests integration-as-defense: performance of care legitimates property while forestalling the deeper challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_integration_as_constraint_weakening, empirical, 'Whether welfare constraints reform or stabilize the property reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__property_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__property_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__property_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(anim_tr_t32, animal_status_kernel__property_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__property_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__property_reading, base_extractiveness, 8, 0.81).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__property_reading, base_extractiveness, 16, 0.84).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__property_reading, base_extractiveness, 24, 0.87).
narrative_ontology:measurement(anim_be_t32, animal_status_kernel__property_reading, base_extractiveness, 32, 0.88).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__property_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__property_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__property_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__property_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(anim_su_t32, animal_status_kernel__property_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__property_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__property_reading, 0.12).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the animal_status_kernel, which is contested across three structurally distinct claims about animal moral status. The property_reading holds that animals are property with no independent moral claims. The welfare_reading holds that property status is permissible if constrained by sentience. The abolitionist_reading holds that property status itself is the injustice. These are not alternative measurements of a single constraint — they have different ε values (property_reading is high-extraction by design; abolitionist_reading sees the extraction as total by stipulating animals are persons; welfare_reading sees moderate extraction from constrained property). Each reading is a separate constraint story linked via network.affects_constraints. The three stories together model the contested kernel and the divergent classifications that result from different framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
