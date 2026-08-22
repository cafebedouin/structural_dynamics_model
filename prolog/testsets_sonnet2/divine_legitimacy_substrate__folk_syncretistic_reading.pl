% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Household/Village Syncretistic Ritual Practice as Divine Legitimacy Substrate
 *   domain: religious/political economy of belief systems
 *
 * SUMMARY:
 *   This story instantiates the folk-syncretistic reading of the divine
 *   legitimacy substrate: households and villages address everyday needs by
 *   pragmatically drawing on whichever deity or ritual specialist has a local
 *   reputation for efficacy, entirely apart from priestly hierarchy or
 *   pharaonic theology. This is structurally distinct from the
 *   amun_polytheistic_reading (legitimacy through priestly interpretation of
 *   an official cosmology anchored on Amun-Ra) and the
 *   atenist_monotheistic_reading (legitimacy through exclusive pharaonic
 *   revelation of Aten). Under this reading, extraction is low and diffuse,
 *   suppression is minimal because the practice sits largely outside the
 *   reach of state or priestly enforcement, and the beneficiary structure is
 *   genuinely unclear — the practice mostly serves the households themselves
 *   rather than concentrating gains in a distant elite. Both the Amun
 *   priesthood and the pharaoh appear here as excluded, distant elites
 *   relative to this substrate, which is the delta the kernel context
 *   specifies.
 *
 * KEY AGENTS:
 *   - household_heads: primary agenda-setters over their own ritual practice (moderate power/mobile exit) — decide which deities to address
 *   - village_ritual_specialists: local beneficiaries who mediate the practice (moderate power/mobile exit) — earn status and modest goods from perceived efficacy
 *   - local_shrine_keepers: beneficiaries maintaining shrines outside state temple economies (moderate power/mobile exit)
 *   - amun_priesthood: excluded institutional elite (institutional power/constrained exit) — claims cosmological authority but cannot penetrate household practice
 *   - pharaoh: excluded institutional elite (institutional power/constrained exit) — claims sole divine conduit but the substrate is indifferent to royal theology
 *   - modern_historians: analytical observers reconstructing the practice from material culture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.18).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.22).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Household/Village Syncretistic Ritual Practice as Divine Legitimacy Substrate").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious/political economy of belief systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '856b64ed-7c21-42b4-8a18-79699c6685a0').
narrative_ontology:cs_kernel_codification('856b64ed-7c21-42b4-8a18-79699c6685a0', implicit).
narrative_ontology:cs_authority_grounding('856b64ed-7c21-42b4-8a18-79699c6685a0', practice).
narrative_ontology:cs_interpretation_layer_present('856b64ed-7c21-42b4-8a18-79699c6685a0').
narrative_ontology:cs_reading_relation('856b64ed-7c21-42b4-8a18-79699c6685a0', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('856b64ed-7c21-42b4-8a18-79699c6685a0', divine_legitimacy_substrate__atenist_monotheistic_reading, influences).
narrative_ontology:cs_axiom('856b64ed-7c21-42b4-8a18-79699c6685a0', foundational, efficacy_grounds_legitimacy_not_lineage).
narrative_ontology:cs_axiom_status(efficacy_grounds_legitimacy_not_lineage, holdable).
narrative_ontology:cs_axiom_grounding('856b64ed-7c21-42b4-8a18-79699c6685a0', efficacy_grounds_legitimacy_not_lineage, instrumental).
narrative_ontology:cs_axiom('856b64ed-7c21-42b4-8a18-79699c6685a0', foundational, divine_access_is_plural_and_unmediated).
narrative_ontology:cs_axiom_status(divine_access_is_plural_and_unmediated, holdable).
narrative_ontology:cs_axiom_grounding('856b64ed-7c21-42b4-8a18-79699c6685a0', divine_access_is_plural_and_unmediated, conventional).
narrative_ontology:cs_reference_frame('856b64ed-7c21-42b4-8a18-79699c6685a0', diffuse_household_ritual_pragmatism).
narrative_ontology:cs_drift_state('856b64ed-7c21-42b4-8a18-79699c6685a0', amarna_period_disruption, gap(stable, minor, false)).
narrative_ontology:cs_created_at('856b64ed-7c21-42b4-8a18-79699c6685a0', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_ritual_specialists).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, local_shrine_keepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, traveling_merchants_and_migrants).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, pragmatic_efficacy_of_ritual_practice).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, plural_divine_accessibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which deities receive offerings at the household shrine based on immediate need — a sick child, a failed harvest, a difficult birth. Assemble a practical pantheon of whichever gods have local reputations for effectiveness in a given domain, switching allegiance without ceremony or doctrinal anxiety when one god 'doesn't answer.' Their authority over the household cult is unchallenged by any external body.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, agenda_setter,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, beneficiary).

% Local wise women, lay priests, and midwife-healers who mediate between households and the diffuse pantheon, receiving food, favors, and status for effective ritual knowledge. They accumulate no formal doctrine and answer to no temple hierarchy; their standing rests entirely on perceived results and community trust, which can evaporate as quickly as it forms.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_ritual_specialists, beneficiary,
    moderate, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, village_ritual_specialists, agenda_setter).

% Maintain small local shrines to household and village deities (ancestors, Bes, Taweret, local genii loci) that operate entirely outside state temple economies. They receive modest offerings in exchange for shrine upkeep and occasional intercession, with no obligation to reconcile their practice with any official theology.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, local_shrine_keepers, beneficiary,
    moderate, generational, mobile, local).

% The state priesthood claims interpretive authority over legitimate divine order but has no practical mechanism to compel household ritual practice, verify household piety, or redirect village offerings toward state temples. From their vantage the folk substrate looks like theological indiscipline they cannot correct — they are structurally locked out of the household cult even while claiming cosmological seniority over it.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, amun_priesthood, excluded,
    institutional, generational, constrained, national).

% Claims to be the sole conduit between the divine order and the land, whether framed as steward of Amun-Ra's cosmos or as sole revealer of Aten. Royal decrees, temple-building, and processional theology reach the capital and major cult centers but do not penetrate the household shrine, where villagers continue addressing whichever deity seems to be listening regardless of the throne's current theological program.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh, excluded,
    institutional, generational, constrained, national).

% Carry foreign or regional deities into new villages along trade routes and are readily absorbed into local pantheons if they prove locally efficacious. Their gods enter the folk substrate without theological vetting, on the same pragmatic terms as any established local deity.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, traveling_merchants_and_migrants, beneficiary,
    powerless, immediate, mobile, regional).

% Reconstruct folk religious practice from votive stelae, amulets, ostraca, and domestic shrine remains that survive independently of state temple records, and note the practice's persistence essentially unchanged across dynastic theological upheavals including the Amarna period.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, modern_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__folk_syncretistic_reading, diffuse).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__folk_syncretistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides households and villages a working, low-cost mechanism for addressing everyday misfortune and uncertainty (illness, childbirth, harvest failure, safe travel) by drawing on whichever deity or ritual practice has local repute for effectiveness in that domain, without requiring doctrinal consistency or centralized sanction.
% TRANSFER_FUNCTION: Modest offerings, food, and social deference move from households to local ritual specialists and shrine keepers in exchange for perceived efficacious mediation; no significant wealth or authority is transferred upward to state temples or the crown.
% ABSENT_VOICES: The Amun priesthood and the pharaonic court would object that this practice lacks theological legitimacy and undermines cosmological order, but they are not participants in household ritual decision-making and have no observed channel to redirect or discipline it.
% DISAPPEARANCE_RATIONALE: From the household perspective the world would rearrange sharply — daily practices addressing illness, birth, and misfortune would lose their functioning mechanism and villagers would have to find new recourse. From the state theological perspective, the practice's disappearance would barely register since it was never counted as legitimate religion in the first place; the state's own account of divine order does not depend on folk practice existing or not.
% FOUNDING_PROBLEM: Ordinary households and villages, far from temple centers and state ritual infrastructure, needed a way to address everyday uncertainty (sickness, birth, crop failure, travel danger) without access to or dependence on official priestly mediation.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological evidence (household shrines, amulets, votive material persisting unchanged across the Amarna disruption) corroborates from outside any priestly or royal account that the practice continued to serve a live function regardless of which theological program the state promoted; no state or priestly source attests to the practice's legitimacy or function, since it lay entirely outside their domain of concern.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, contested).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because no concentrated party captures rents from the practice — offerings flow to local specialists roughly commensurate with perceived services rendered, and there is no institutional apparatus skimming a surplus. Suppression is low (0.22) because nothing coerces households into this practice or forecloses alternatives; it persists because it is useful, not because exit is blocked. Theater ratio is low (0.12) — the practice is not performative maintenance of a hollowed function, it is the live function itself. Resistance is moderate (0.35) reflecting occasional friction with state theological programs (e.g., Amarna-period pressure) without ever actually suppressing the underlying practice, which is precisely why the practice survives Akhenaten's reforms essentially unchanged. Accessibility collapse is low (0.15): villagers retained the option to add, drop, or substitute deities freely throughout the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Household heads and village ritual specialists sit near the beneficiary end of directionality — the practice subsidizes their immediate needs and local standing at low cost. There are no clear victims in this reading; the beneficiary structure is deliberately left diffuse per the kernel delta, because the practice does not concentrate extraction on any identifiable payer class. The excluded elites (priesthood, pharaoh) are not victims either — they simply lack access to a substrate that was never built to serve or be governed by them, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (household recourse against everyday uncertainty absent elite ritual infrastructure) remains live throughout the interval — it does not decay into a hollow mandate because the practice never depended on state sanction to begin with. There is no mandatrophy to resolve here: the practice's function and its justification remain identical across the measured interval, which is itself evidence against classifying this as extractive machinery masquerading as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    folk_practice_beneficiary_diffuseness,
    'Does the folk-syncretistic substrate genuinely lack a concentrated beneficiary class, or do village ritual specialists constitute an under-described extractive elite at smaller scale?',
    'Comparative analysis of offering volumes and social capital accrued by ritual specialists relative to average household wealth, using material culture (grave goods, shrine furnishings) as a wealth proxy across villages.',
    'If ritual specialists show significant wealth or status accumulation disproportionate to service rendered, the reading shifts toward a mild tangled_rope at the local level rather than a clean rope; if not, the diffuse-beneficiary reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_practice_beneficiary_diffuseness, empirical, 'Whether village-level ritual specialists constitute a hidden extractive class.').

omega_variable(
    state_indifference_vs_incapacity,
    'Does the state/priestly apparatus tolerate the folk substrate because it is doctrinally indifferent to it, or because it structurally lacks the capacity to suppress it despite wanting to?',
    'Examine surviving royal and priestly decrees for explicit reference to household/village ritual practice; absence of any mention across multiple dynasties would support genuine indifference over failed suppression.',
    'If suppression was attempted and failed, the low suppression score understates the constraint''s true contest history and resistance should be revised upward; if genuinely never contested, current scores hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_indifference_vs_incapacity, empirical, 'Whether elite non-intervention reflects indifference or incapacity.').

omega_variable(
    cs_framing_household_vs_syncretic_narrative,
    'Is the more defensible framing the household ritual practice itself (as authored here), or the layered syncretic narrative tradition that later retrospectively organizes these practices into a coherent ''popular religion'' category?',
    'Compare classification results treating the kernel as the raw practice versus treating it as the scholarly/emic narrative category of ''folk religion'' that imposes retrospective coherence on scattered practices.',
    'Framing as raw practice (chosen here) yields a low-authority, low-codification, practice-grounded reading (implicit kernel, practice authority). Framing as the retrospective narrative category would shift authority_grounding toward distributed/expertise (modern scholarly reconstruction) and could inflate apparent coherence the ancient participants never claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_household_vs_syncretic_narrative, conceptual, 'Alternative framing: raw household practice versus retrospectively organized ''folk religion'' category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 100, 0.11).
narrative_ontology:measurement(divi_tr_t150, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 150, 0.11).
narrative_ontology:measurement(divi_tr_t200, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(divi_tr_t250, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 250, 0.12).
narrative_ontology:measurement(divi_tr_t300, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 300, 0.12).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 50, 0.17).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 100, 0.17).
narrative_ontology:measurement(divi_be_t150, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 150, 0.18).
narrative_ontology:measurement(divi_be_t200, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 200, 0.18).
narrative_ontology:measurement(divi_be_t250, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 250, 0.18).
narrative_ontology:measurement(divi_be_t300, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 300, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(divine_legitimacy_substrate__folk_syncretistic_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__folk_syncretistic_reading, 0.1).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% Part of the divine_legitimacy_substrate kernel family (3 readings): amun_polytheistic_reading (priestly-interpretive authority over official cosmology), atenist_monotheistic_reading (exclusive pharaonic revelation), and this folk_syncretistic_reading (diffuse household/village practice). Each reading is authored as its own constraint with its own ε — the folk reading's low extractiveness (0.18) reflects a genuinely different structural claim than the priestly or pharaonic readings, not a different measurement of the same claim. The folk substrate's stability across the Amarna disruption (which sharply perturbs the pharaonic reading) is the key evidence that these are structurally distinct constraints, not one constraint viewed at different scales.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
