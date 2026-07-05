% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Legitimate Prophetic Reinterpretation Suspending Plural Marriage
 *   domain: religious/institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates the endogenous reinterpretation reading of the
 *   plural-marriage-mandate kernel: the 1890 Manifesto is read from within
 *   the church's own theological framework as an authentic act of continuing
 *   revelation, in which God suspended (without permanently repudiating) the
 *   practice of plural marriage in order to preserve the institution's
 *   capacity to carry out its salvific mission — temple ordinances,
 *   missionary work, and communal survival. Under this reading, the
 *   coordination function is genuine: a single prophetic directive resolves a
 *   crisis that threatened to fracture and destroy the institution, and the
 *   vast majority of the church body accepts and benefits from the
 *   resolution. The victim set is real but narrower than the sibling readings
 *   would locate it: fundamentalist dissenters who hold the original
 *   revelation as permanently binding are treated, from within this reading,
 *   not as victims of coercion but as those who reject legitimate continuing
 *   revelation — yet structurally they still bear the cost (excommunication,
 *   loss of temple standing, legal jeopardy) of the arrangement's
 *   enforcement. This is why the constraint computes closer to rope than
 *   tangled_rope from most seats even though a genuine victim group exists:
 *   the coordination function dominates, extraction is comparatively low, and
 *   the victims are a bounded, doctrinally self-selected minority rather than
 *   a broad exploited class. Sibling constraints (exogenous_override_reading,
 *   institutional_pragmatism_reading) model the same historical episode with
 *   different beneficiary/victim structures and different epsilon values —
 *   see kernel_context.
 *
 * KEY AGENTS:
 *   - church_leadership_hierarchy: agenda_setter (institutional/analytical) — issues and enforces the Manifesto as revelation
 *   - church_institution: beneficiary/agenda_setter (institutional/arbitrage) — survives and consolidates via the new directive
 *   - mainstream_church_members: beneficiary (moderate/constrained) — regain temple access and social normalization
 *   - fundamentalist_dissenters: payer (powerless/trapped) — excommunicated for maintaining the original reading
 *   - excommunicated_plural_families: payer (powerless/trapped) — bear direct relational and communal cost
 *   - federal_government: excluded (institutional/analytical) — causal pressure absent from the internal doctrinal account
 *   - religious_historians: observer (analytical/analytical) — assess the endogenous claim against the historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.42).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.58).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto as Legitimate Prophetic Reinterpretation Suspending Plural Marriage").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious/institutional/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, 'b9d5beb8-3ebb-4000-ab98-82d4a60f56d6').
narrative_ontology:cs_kernel_codification('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', formalized).
narrative_ontology:cs_authority_grounding('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', lineage).
narrative_ontology:cs_interpretation_layer_present('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6').
narrative_ontology:cs_reading_relation('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', foundational, continuing_revelation_can_temporally_suspend_prior_command).
narrative_ontology:cs_axiom_status(continuing_revelation_can_temporally_suspend_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', continuing_revelation_can_temporally_suspend_prior_command, theological).
narrative_ontology:cs_axiom('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', secondary, prophetic_authority_supersedes_prior_textual_command_without_repudiating_it).
narrative_ontology:cs_axiom_status(prophetic_authority_supersedes_prior_textual_command_without_repudiating_it, holdable).
narrative_ontology:cs_axiom_grounding('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', prophetic_authority_supersedes_prior_textual_command_without_repudiating_it, conventional).
narrative_ontology:cs_reference_frame('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', continuing_revelation_prophetic_succession).
narrative_ontology:cs_drift_state('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', post_manifesto_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b9d5beb8-3ebb-4000-ab98-82d4a60f56d6', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_church_members).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_leadership_hierarchy).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, excommunicated_plural_families).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and subsequently enforces the Manifesto as binding revelation, reorganizes church governance around monogamy compliance, and administers excommunication proceedings against those who continue plural marriage. Frames the shift as prophetic continuity rather than doctrinal reversal, preserving its own authority to declare what counts as revealed truth going forward.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_leadership_hierarchy, agenda_setter,
    institutional, civilizational, analytical, national).

% Gain restored access to temple ordinances, statehood integration, reduced federal harassment, and social normalization within broader American society. Accept the reinterpretation as authoritative because it resolves a decades-long crisis and because leaving the faith community over this question is costly to identity and kinship networks.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_church_members, beneficiary,
    moderate, generational, constrained, national).

% As an organization, survives federal prosecution, asset seizure threats, and territorial disenfranchisement by adopting the new directive. Retains doctrinal ownership of plural marriage as an eternal principle (unpracticed but not repudiated), preserving future interpretive flexibility while securing institutional continuity, statehood, and missionary access.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, agenda_setter).

% Read the original revelation (Doctrine and Covenants 132) as a permanent, non-negotiable commandment and view the Manifesto as institutional betrayal rather than legitimate updated revelation. Continue the practice at severe personal risk, facing excommunication, loss of community standing, and legal prosecution; their exit from mainstream church structures is total and their reentry into the practice-continuing subculture is their only remaining community option.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters, payer,
    powerless, generational, trapped, local).

% Bear the direct cost of the reinterpretation: severed from the institution's temple ordinances, genealogical rites, and social recognition after being formally cast out for maintaining what they hold to be the unaltered divine command. Many are multi-generational descendants of practicing families with no alternative institutional home other than splinter fundamentalist sects.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, excommunicated_plural_families, payer,
    powerless, biographical, trapped, local).

% Applied the coercive pressure (Edmunds-Tucker Act, disincorporation threats, disenfranchisement) that immediately preceded the Manifesto, but is not treated within this reading's own theological account as a causal factor — the reading positions revelation, not federal pressure, as the operative cause, so the federal role is present in the historical record but absent from the internal doctrinal narrative that legitimates the change.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, biographical, analytical, national).

% Examine church records, correspondence, and the timing correlation between federal legal pressure and the revelation's announcement to assess whether the endogenous account holds independently of coercive circumstance.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, religious_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, binding resolution to a doctrinal-practical crisis, allowing the institution's members, leadership, and external relations to reorganize around one settled directive instead of fracturing over contested revelation, thereby preserving temple access, missionary work, and corporate continuity.
% TRANSFER_FUNCTION: Moves institutional legitimacy and survival capacity toward the mainstream church body and its leadership, while moving communal belonging, doctrinal standing, and temple access away from those who maintain the pre-1890 practice as binding.
% ABSENT_VOICES: Fundamentalist dissenters and their descendants would object that a temporally convenient revelation arriving amid existential federal pressure lacks the independent theological grounding claimed for it; they are not represented within mainstream institutional decision-making after 1890 and their objections are addressed only through excommunication proceedings, not doctrinal dialogue.
% DISAPPEARANCE_RATIONALE: If this reading's authority were withdrawn — if the church itself repudiated the Manifesto as illegitimate — the institution's claim to continuous prophetic authority would collapse, temple recognition of post-1890 monogamous marriages would be thrown into doctrinal question, and the excommunication of fundamentalist groups would lose its theological basis, likely triggering reconciliation efforts or renewed schism.
% FOUNDING_PROBLEM: The church faced imminent legal dissolution: federal seizure of church property, disenfranchisement of practicing members, and a blocked path to Utah statehood, all contingent on ending plural marriage as an institutionally sanctioned practice.
% FOUNDING_PROBLEM_CORROBORATION: The federal pressure that constituted the founding problem is not attested by any party as still live — the Edmunds-Tucker Act and associated prosecutions ended with the Manifesto and Utah statehood. Independent historians (outside both the church hierarchy and the fundamentalist dissenting groups) corroborate that the coercive legal crisis was real and resolved; what remains contested only between the sibling readings is whether the SOLUTION to that dead problem was itself an independent revelation or a pragmatic response to it.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than low or high: within this reading the arrangement genuinely coordinates a large population around a resolved crisis (pulling toward rope), but active enforcement against dissenters (excommunication, denial of temple access, later intensified opposition to fundamentalist splinter groups) introduces a real extractive component that keeps epsilon above a pure-coordination floor. Suppression rises sharply at the 1890 inflection point (0.3 to 0.55) as excommunication becomes the enforcement mechanism against continued practice, then stabilizes near 0.58-0.65 as fundamentalist groups are pushed into permanent exclusion rather than reconciled. Theater ratio climbs modestly (0.10 to 0.28) reflecting the institution's growing emphasis on affirming the Manifesto's continuing-revelation framing through commemorative and doctrinal reaffirmation even as the underlying crisis recedes into history. Accessibility collapse (0.62) reflects that once the Manifesto is accepted as authoritative revelation within the institution, alternative doctrinal readings become practically inaccessible to members in good standing — though not absolutely, since fundamentalist alternatives persist outside the institution.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership and the institution itself sit at the strong-beneficiary end: they set the new directive, retain interpretive authority over it, and secure long-term survival and growth. Mainstream members are beneficiaries with some constraint on exit (leaving the faith carries real relational costs, but staying required no self-sacrifice comparable to the dissenters). Fundamentalist dissenters and excommunicated plural families sit at the target end: trapped by identity and community investment in the original doctrine, they bear excommunication and social severance as the direct cost of the institution's directional shift. The federal government is excluded from this reading's own directionality calculus by construction — the reading's theological claim is precisely that revelation, not federal coercion, was the operative cause, so federal pressure is a historically present but doctrinally unacknowledged factor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imminent federal dissolution of the church) is dead — resolved decisively by 1896 statehood. Yet under this reading, the church's authority to declare the resolution wasn't a mandate that outlived its function so much as an authority structure that successfully transitioned its mandate: the classification as rope (not piton) reflects that the coordination function — a single settled doctrinal position enabling continued institutional operation — remains genuinely load-bearing today (temple recognition, missionary framing, doctrinal coherence), not merely inertial. This prevents mislabeling ongoing doctrinal administration as pure theater: the mainstream church continues to derive real coordination value from the settled position, distinguishing this from a Piton where only performance remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_vs_coercion_timing,
    'Is the temporal correlation between federal legal pressure (Edmunds-Tucker Act enforcement, imminent disincorporation) and the announcement of the Manifesto evidence against the endogenous reading''s claim of independent revelation, or is correlation compatible with God acting providentially through circumstance?',
    'This is not resolvable by external evidence in principle, since it turns on a claim about divine action that is unfalsifiable from a secular historical standpoint; internal church historical scholarship (journal entries, private correspondence of church president Wilford Woodruff around September 1890) can establish the sincerity of the claimed revelatory experience but cannot establish its metaphysical authenticity.',
    'If the internal record shows the revelation claim was constructed after the fact purely as institutional messaging, this reading collapses into institutional_pragmatism_reading; if the record shows genuine independent spiritual conviction predating or independent of the immediate legal crisis, the endogenous reading is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity_vs_coercion_timing, conceptual, 'Whether documented timing correlation undermines or is compatible with genuine revelation claim.').

omega_variable(
    doctrine_retained_vs_repudiated,
    'Does the church''s retention of plural marriage as an unrepudiated ''eternal principle'' (merely suspended, not doctrinally false) constitute continuity that supports the endogenous reading, or is it evidence of institutional hedging that better fits the pragmatism reading?',
    'Compare subsequent official statements (1904 Second Manifesto, 20th-century doctrinal clarifications) for whether the church treats the underlying principle as true-but-suspended or has quietly moved toward treating it as doctrinally superseded.',
    'Continued formal non-repudiation over a century supports the endogenous claim of principled temporal suspension; any drift toward doctrinal disavowal would suggest retroactive rationalization consistent with the pragmatism reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_retained_vs_repudiated, empirical, 'Whether formal doctrinal retention is genuine principle or institutional hedge.').

omega_variable(
    fundamentalist_victim_scope,
    'Should the victim set be bounded to those who actively continued the practice post-1890 and were excommunicated, or does it extend more broadly to all members whose doctrinal commitments were unilaterally altered without their consent?',
    'Survey the range of contemporaneous member responses (diaries, ward records, apostate accounts) to determine whether broad member acquiescence reflected genuine acceptance of new revelation or resigned compliance under institutional and social pressure.',
    'A narrow victim scope (only active practitioners) supports the rope classification claimed here; a broad scope (all members whose prior doctrinal commitment was overridden) would push the constraint toward tangled_rope even within this endogenous framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentalist_victim_scope, empirical, 'How broadly the victim class should be drawn within the endogenous reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1880, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(plur_tr_t1915, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1915, 0.25).
narrative_ontology:measurement(plur_tr_t1930, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1930, 0.28).
narrative_ontology:measurement(plur_tr_t1950, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1950, 0.28).

% Extraction over time
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1880, 0.2).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(plur_be_t1915, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1915, 0.42).
narrative_ontology:measurement(plur_be_t1930, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1930, 0.42).
narrative_ontology:measurement(plur_be_t1950, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1950, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1880, 0.3).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.55).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(plur_su_t1915, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1915, 0.6).
narrative_ontology:measurement(plur_su_t1930, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1930, 0.58).
narrative_ontology:measurement(plur_su_t1950, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1950, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.1).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the plural_marriage_mandate kernel. endogenous_reinterpretation_reading (this file) claims moderate extraction (0.42) and a rope classification grounded in genuine continuing-revelation authority; exogenous_override_reading claims higher extraction and a narrower coordination function (federal coercion overriding a still-binding command, victims broadened to all practicing members); institutional_pragmatism_reading claims the revelation narrative is cover for survival-driven capitulation, likely computing as tangled_rope or snare with the church leadership recast as extracting legitimacy from a manufactured doctrinal story. All three share the same historical event (the 1890 Manifesto) but diverge irreducibly on the causal and theological status of the change — per the ε-invariance principle, this is three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
