% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism â Composite Reading
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint is the composite_reading of the contested kernel
 *   honor_satisfaction_mechanism, which concerns how European societies
 *   regulated honor disputes from the early modern period through the
 *   nineteenth century. The composite reading holds that the mechanism eroded
 *   through multiple independent pressuresâstate monopoly on violence,
 *   bourgeois normative systems, insurance rationalization, and structural
 *   category-shiftârather than through a single dominant cause. The
 *   constraint story treats the honor satisfaction mechanism as a tangled
 *   rope: it genuinely coordinated aristocratic violence to prevent feuds,
 *   yet asymmetrically extracted bodily risk, wealth, and deference from
 *   non-aristocrats and women. By the interval's end, the mechanism had
 *   undergone resolved mandatrophy.
 *
 * KEY AGENTS:
 *   - gentleman_aristocrats: Primary agenda-setter and beneficiary (powerful/mobile) â administers the code and captures status reproduction
 *   - bourgeois_merchants: Primary payer (moderate/constrained) â adopts aristocratic rituals at personal cost to secure commercial standing
 *   - working_class_men: Secondary payer (powerless/trapped) â excluded from honor protections, subject to unregulated violence
 *   - women: Structural payer (powerless/trapped) â objectified as honor stakes without procedural voice
 *   - state_judiciary: Institutional observer (institutional/analytical) â competes for monopoly on legitimate violence
 *   - bourgeois_reformers: Excluded voice (organized/constrained) â advocates legal-rational replacement, barred from honor councils
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.62).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.58).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism â Composite Reading").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '85611401-2a41-4b4d-8253-d079483aacc7').
narrative_ontology:cs_kernel_codification('85611401-2a41-4b4d-8253-d079483aacc7', fixed_text).
narrative_ontology:cs_authority_grounding('85611401-2a41-4b4d-8253-d079483aacc7', lineage).
narrative_ontology:cs_interpretation_layer_present('85611401-2a41-4b4d-8253-d079483aacc7').
narrative_ontology:cs_reading_relation('85611401-2a41-4b4d-8253-d079483aacc7', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('85611401-2a41-4b4d-8253-d079483aacc7', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('85611401-2a41-4b4d-8253-d079483aacc7', foundational, erosion_via_multiple_independent_mechanisms).
narrative_ontology:cs_axiom_status(erosion_via_multiple_independent_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('85611401-2a41-4b4d-8253-d079483aacc7', erosion_via_multiple_independent_mechanisms, empirically_contingent).
narrative_ontology:cs_axiom('85611401-2a41-4b4d-8253-d079483aacc7', foundational, structural_recategorization_of_violence).
narrative_ontology:cs_axiom_status(structural_recategorization_of_violence, holdable).
narrative_ontology:cs_axiom_grounding('85611401-2a41-4b4d-8253-d079483aacc7', structural_recategorization_of_violence, empirically_contingent).
narrative_ontology:cs_reference_frame('85611401-2a41-4b4d-8253-d079483aacc7', aristocratic_honor_dispute_resolution).
narrative_ontology:cs_drift_state('85611401-2a41-4b4d-8253-d079483aacc7', bourgeois_state_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('85611401-2a41-4b4d-8253-d079483aacc7', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, gentleman_aristocrats).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, bourgeois_merchants).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, working_class_men).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, women).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, aristocratic_supremacy_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, masculine_honor_codification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the codes of honor, served as seconds, and adjudicated disputes through private courts of honor. Used the mechanism to reproduce aristocratic status boundaries and maintain exclusive control over legitimate interpersonal violence. Could theoretically exit by refusing to duel, but doing so risked social death within their class.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, gentleman_aristocrats, agenda_setter,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, gentleman_aristocrats, beneficiary).

% Adopted dueling and honor rituals to secure gentleman status and commercial trust, bearing significant costs in time, injury risk, and conspicuous display. Structurally pressured to conform to aristocratic norms or face exclusion from elite credit and marriage networks.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_merchants, payer,
    moderate, biographical, constrained, national).

% Excluded from honorable combat protections; subject to unregulated violence, criminal punishment, or military impressment. Could not invoke honor codes to shield themselves from aristocratic or state violence, yet bore the physical consequences of aristocratic dispute rituals.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, working_class_men, payer,
    powerless, immediate, trapped, local).

% Objectified as the stakes of masculine honor disputes; suffered reputational destruction, physical violence, and property loss as consequences of duels and honor rituals in which they had no procedural voice. Their chastity and reputation were tradable commodities within the gentleman code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, women, payer,
    powerless, biographical, trapped, national).

% Administered a competing monopoly on legitimate violence through courts and criminal law. Observed the honor mechanism as an illegal rival system, prosecuting some duels while strategically tolerating others until state consolidation permitted systematic suppression.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_judiciary, observer,
    institutional, generational, analytical, national).

% Advocated for the replacement of private violence with rational-legal dispute resolution and insurance mechanisms. Excluded from aristocratic honor councils and courts of honor; their objections were dismissed as philistine or cowardly by the agenda-setting class.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_reformers, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, gentleman_aristocrats).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a codified, bounded ritual for resolving interpersonal honor disputes among aristocratic men, substituting regulated single combat and social ostracism for kin-based blood feuds and anarchic retaliatory violence.
% TRANSFER_FUNCTION: Transferred blood, wealth, time, and social deference from non-gentlemen and compliant bourgeois aspirants to the aristocratic class; transferred dispute resolution authority from kin groups and state courts to gentleman seconds and private courts of honor.
% ABSENT_VOICES: Women, whose reputations and bodies were the stakes but who had no voice in the codes; working-class men, who were denied honorable combat status and subject to unregulated violence; bourgeois reformers and state prosecutors, who opposed the private violence monopoly but were excluded from aristocratic honor councils.
% DISAPPEARANCE_RATIONALE: If the honor satisfaction mechanism vanished overnight, aristocratic status reproduction would lose a key boundary marker, bourgeois men would be released from costly adoption of aristocratic rituals, women would cease to be objectified as honor stakes, and violence would shift entirely to state courts or unregulated crime â the social order would reorganize around legal-rational dispute resolution.
% FOUNDING_PROBLEM: Weak early-modern state capacity and persistent kin-based feud systems produced unregulated cycles of retaliatory violence that threatened aristocratic social order and property.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists (Elias, Spierenburg) from outside the aristocratic beneficiary class attest the founding problem was real but solved by state consolidation and civilizing processes by the mid-19th century; state judicial records and bourgeois reform literature corroborate that the problem was subsumed under legal frameworks, while aristocratic memoirs alone claim the problem persisted.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the mechanism transferred blood, wealth, and status asymmetrically to the gentleman class while excluding non-aristocrats. Suppression (0.58) reflects active social enforcement through ostracism and ritual institutions. Theater ratio (0.48) captures the increasing performativity as the mechanism atrophied under state and bourgeois pressure. Accessibility collapse (0.65) is high because alternatives (state courts) were culturally suppressed for gentlemen until the mechanism eroded. Resistance (0.55) reflects persistent bourgeois and state opposition. Measurements use one shared time grid spanning the erosion arc.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic seat experiences the mechanism as legitimate coordination preventing chaos and preserving civilization; the bourgeois and working-class seats experience it as enforced extraction of deference and bodily risk; the state seat experiences it as a competitor to judicial monopoly. The engine computes these divergences from the structural position data â the agenda-setter has mobile exit and generational time horizon, while payers are trapped or constrained with immediate or biographical horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentleman aristocrats are the structural beneficiaries and agenda-setters (d near 0.0): they collect status reproduction, dispute resolution authority, and class-boundary enforcement. Bourgeois merchants, working-class men, and women are the payers (d near 1.0): they bear the costs of violence adoption, unregulated violence exposure, and objectification respectively. State judiciary sits at analytical distance (d near 0.5) as observer of a competing system. No override is needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunregulated feud violence in a weak-state environmentâwas solved by state consolidation, insurance markets, and bourgeois legal-rational norms by the late nineteenth century. The composite reading captures this resolved mandatrophy by identifying multiple independent erosion mechanisms. Classification as tangled_rope prevents mislabeling the early coordination function as pure extraction, while founding_problem_status=dead paired with disappearance_verdict=world_rearranges flags the post-functional persistence. The temporal measurements show extraction peaking under defensive pressure before collapsing, while theater_ratio rises monotonically toward the piton-attractor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    composite_reading_kernel_location,
    'This constraint is the composite_reading of kernel honor_satisfaction_mechanism. Sibling readings include decline_reading (persistence with declining frequency) and contraction_reading (cognitive unthinkability). What would change structurally if decline_reading were adopted as the sole framing?',
    'Comparative historiographical analysis testing whether the mechanisms are truly independent or reducible to frequency decline.',
    'Would collapse the multicausal claim into a single descriptive pattern, potentially masking structural recategorization and treating category-shift as epiphenomenal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_reading_kernel_location, conceptual, 'Structural delta between composite and sibling decline reading.').

omega_variable(
    multicausal_erosion_or_dominant_state,
    'Was the erosion of honor satisfaction genuinely driven by multiple independent mechanisms (state monopoly, bourgeois norms, insurance, category-shift), or was state monopoly on violence the dominant cause with other factors as secondary?',
    'Comparative historical analysis across jurisdictions with varying state capacity, insurance penetration, and bourgeois political power.',
    'If state monopoly dominated, classification shifts toward enforcement decay; if multicausal, the constraint faced distributed erosion consistent with tangled_rope decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multicausal_erosion_or_dominant_state, empirical, 'Whether erosion was multicausal or state-dominant.').

omega_variable(
    structural_vs_internalized_suppression,
    'Was the suppression sustaining honor satisfaction primarily structural (social ostracism, legal impunity for gentlemen) or internalized (shame, masculine identity fusion)?',
    'Post-erosion trajectory analysis: if dueling norms persisted in identity discourse after structural incentives vanished, internalization was significant.',
    'If internalized, effective suppression exceeds structural measure and explains persistence beyond functional utility; if structural, disappearance tracks institutional change cleanly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Structural vs internalized suppression mechanism in honor codes.').

omega_variable(
    category_shift_cause_or_effect,
    'Did cognitive recategorization of dueling as ''unthinkable'' cause institutional decline, or did institutional decline permit cognitive recategorization?',
    'Discourse analysis of gentleman periodicals and legal treatises tracking when category language shifted relative to enforcement events.',
    'If category-shift was causal, the constraint''s accessibility_collapse was driven by ideational change; if effect, by material enforcement decay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_shift_cause_or_effect, conceptual, 'Direction of causality for category-shift mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_sat_comp_tr_t0, honor_satisfaction_mechanism__composite_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(honor_sat_comp_tr_t20, honor_satisfaction_mechanism__composite_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(honor_sat_comp_tr_t40, honor_satisfaction_mechanism__composite_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(honor_sat_comp_tr_t60, honor_satisfaction_mechanism__composite_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(honor_sat_comp_tr_t80, honor_satisfaction_mechanism__composite_reading, theater_ratio, 80, 0.65).
narrative_ontology:measurement(honor_sat_comp_tr_t100, honor_satisfaction_mechanism__composite_reading, theater_ratio, 100, 0.78).

% Extraction over time
narrative_ontology:measurement(honor_sat_comp_be_t0, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(honor_sat_comp_be_t20, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(honor_sat_comp_be_t40, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(honor_sat_comp_be_t60, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(honor_sat_comp_be_t80, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(honor_sat_comp_be_t100, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 100, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(honor_sat_comp_su_t0, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(honor_sat_comp_su_t20, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(honor_sat_comp_su_t40, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(honor_sat_comp_su_t60, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(honor_sat_comp_su_t80, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(honor_sat_comp_su_t100, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 100, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% The kernel honor_satisfaction_mechanism decomposes into three constraint stories (composite_reading, decline_reading, contraction_reading) because the natural-language label 'dueling declined' conflates structurally distinct historiographical claims about mechanism, frequency, and cognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
