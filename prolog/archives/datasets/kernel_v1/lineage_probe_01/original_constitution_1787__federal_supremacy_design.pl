% ============================================================================
% CONSTRAINT STORY: original_constitution_1787__federal_supremacy_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_original_constitution_1787__federal_supremacy_design, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: original_constitution_1787__federal_supremacy_design
 *   human_readable: Federal Supremacy Design (1787 Constitution Reading)
 *   domain: political/legal/constitutional_law
 *
 * SUMMARY:
 *   The federal supremacy reading of the 1787 Constitution identifies the
 *   core structural innovation as replacing the confederal league (where
 *   states retained sovereignty and could refuse compliance) with a national
 *   government that acts directly on individuals through supreme law. This
 *   reading emphasizes the Supremacy Clause (Article VI, Clause 2) as the
 *   engine of extraction: state laws are subordinated to federal law and
 *   federal treaties; the federal judiciary enforces this hierarchy; states
 *   cannot nullify federal acts. The primary beneficiary is national
 *   commercial interests seeking uniform law and interstate market access.
 *   The primary victim set is state-level majorities overridden in their own
 *   jurisdictions. The constraint is classified as Tangled Rope because it
 *   exhibits both genuine coordination (solving interstate commerce problems,
 *   providing common defense) and asymmetric extraction (centralizing power
 *   in federal structures that disproportionately serve commercial
 *   interests). The measurement trajectory shows extraction rising from 0.42
 *   to 0.65 over the first 40 years as federal enforcement mechanisms
 *   (judiciary, taxation, military) become operative and suppress state
 *   nullification attempts (Kentucky and Virginia Resolutions, early
 *   secessionist doctrine). This is one of four competing readings of the
 *   same 1787 text; the sibling readings identify Article V revisability,
 *   separation of powers, and slavery compromises as the core commitments
 *   instead.
 *
 * KEY AGENTS:
 *   - National Commercial Interests: Primary beneficiary (institutional/arbitrage) — gain uniform law, eliminated tariff barriers, enforced contracts; experience constraint as coordination
 *   - State-Level Majorities: Primary victim (powerless/trapped) — overridden in their own jurisdictions; cannot exit or nullify federal supremacy
 *   - State Governments: Organized actors (organized/constrained) — constrained by supremacy but benefit from coordinated union and retain police power
 *   - Federal Government: Institutional actor (institutional/constrained) — benefits from centralized extraction capacity but remains checked by constitutional limits
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing federal supremacy as immutable structural necessity rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(original_constitution_1787__federal_supremacy_design, 0.58).
domain_priors:suppression_score(original_constitution_1787__federal_supremacy_design, 0.68).
domain_priors:theater_ratio(original_constitution_1787__federal_supremacy_design, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(original_constitution_1787__federal_supremacy_design, extractiveness, 0.58).
narrative_ontology:constraint_metric(original_constitution_1787__federal_supremacy_design, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(original_constitution_1787__federal_supremacy_design, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(original_constitution_1787__federal_supremacy_design, tangled_rope).
narrative_ontology:human_readable(original_constitution_1787__federal_supremacy_design, "Federal Supremacy Design (1787 Constitution Reading)").
narrative_ontology:topic_domain(original_constitution_1787__federal_supremacy_design, "political/legal/constitutional_law").

domain_priors:requires_active_enforcement(original_constitution_1787__federal_supremacy_design).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(original_constitution_1787__federal_supremacy_design, '1f756e57-8142-4291-91ee-0963ebe189ff').
narrative_ontology:cs_kernel_codification('1f756e57-8142-4291-91ee-0963ebe189ff', fixed_text).
narrative_ontology:cs_authority_grounding('1f756e57-8142-4291-91ee-0963ebe189ff', lineage).
narrative_ontology:cs_interpretation_layer_present('1f756e57-8142-4291-91ee-0963ebe189ff').
narrative_ontology:cs_reading_relation('1f756e57-8142-4291-91ee-0963ebe189ff', original_constitution_1787__article_v_amendment_procedure, coexists_with).
narrative_ontology:cs_reading_relation('1f756e57-8142-4291-91ee-0963ebe189ff', original_constitution_1787__separation_of_powers_design, coexists_with).
narrative_ontology:cs_reading_relation('1f756e57-8142-4291-91ee-0963ebe189ff', original_constitution_1787__slavery_compromises, influences).
narrative_ontology:cs_axiom('1f756e57-8142-4291-91ee-0963ebe189ff', foundational, federal_law_supreme_over_state_law).
narrative_ontology:cs_axiom_status(federal_law_supreme_over_state_law, holdable).
narrative_ontology:cs_axiom_grounding('1f756e57-8142-4291-91ee-0963ebe189ff', federal_law_supreme_over_state_law, deontological).
narrative_ontology:cs_axiom('1f756e57-8142-4291-91ee-0963ebe189ff', foundational, national_government_acts_directly_on_individuals).
narrative_ontology:cs_axiom_status(national_government_acts_directly_on_individuals, holdable).
narrative_ontology:cs_axiom_grounding('1f756e57-8142-4291-91ee-0963ebe189ff', national_government_acts_directly_on_individuals, instrumental).
narrative_ontology:cs_reference_frame('1f756e57-8142-4291-91ee-0963ebe189ff', confederal_subordination_replaced_by_federal_supremacy).
narrative_ontology:cs_drift_state('1f756e57-8142-4291-91ee-0963ebe189ff', contemporary_regulatory_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1f756e57-8142-4291-91ee-0963ebe189ff', '').
narrative_ontology:cs_kernel_id(original_constitution_1787__federal_supremacy_design, original_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(original_constitution_1787__federal_supremacy_design, national_commercial_interests).
narrative_ontology:constraint_beneficiary(original_constitution_1787__federal_supremacy_design, federal_government).
narrative_ontology:constraint_victim(original_constitution_1787__federal_supremacy_design, state_level_majorities).
narrative_ontology:constraint_victim(original_constitution_1787__federal_supremacy_design, local_autonomy_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE-LEVEL MAJORITIES (SNARE) — Cannot exit the federal union without secession (legal nullity in 1787 framework). State legislatures face supremacy clause enforcement through federal judiciary. Their decisions are subordinated to national law with no veto mechanism. Trapped extraction with suppression of nullification doctrines.
constraint_indexing:constraint_classification(original_constitution_1787__federal_supremacy_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GOVERNMENTS AS ORGANIZED ACTORS (TANGLED ROPE) — Genuinely constrained but not trapped. States retain police power, taxation authority, and legislative domain. They also benefit from federal protection (common defense, treaty-making power). The constraint is mixed: federal supremacy limits their sovereignty, but coordinated union provides collective goods (interstate commerce regulation, military capacity). Effective extraction moderated by genuine coordination function.
constraint_indexing:constraint_classification(original_constitution_1787__federal_supremacy_design, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATIONAL COMMERCIAL INTERESTS (ROPE) — Primary beneficiary. Federal supremacy enables uniform commercial law, eliminates tariff barriers between states, and provides enforced contract claims through federal judiciary. These interests experience the constraint as pure coordination: it solves collective action problems of interstate commerce. The beneficiary's arbitrage options include regulatory migration within the federal system.
constraint_indexing:constraint_classification(original_constitution_1787__federal_supremacy_design, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT (TANGLED ROPE) — The federal structure both enables and constrains federal power. Federal supremacy grants centralized authority (taxation, regulation, judiciary enforcement) but checks exist: federalism floor (state police power), separation of powers (congressional supermajorities for taxation), limited enumeration of powers. The federal government benefits from extraction capacity but remains constrained by the constitutional structure it wields.
constraint_indexing:constraint_classification(original_constitution_1787__federal_supremacy_design, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (MOUNTAIN) — From a civilizational scale, federal supremacy appears as an immutable structural requirement for continental union. The alternatives (confederal league, mutual consent of states) are presented as inherently unstable. This perspective risks naturalizing what is actually a contingent institutional choice made by specific agents with specific interests. FALSE SUMMIT CANDIDATE: beneficiaries are identifiable (national commercial interests), and the constraint relies on suppression of state nullification doctrines.
constraint_indexing:constraint_classification(original_constitution_1787__federal_supremacy_design, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(original_constitution_1787__federal_supremacy_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(original_constitution_1787__federal_supremacy_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(original_constitution_1787__federal_supremacy_design, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(original_constitution_1787__federal_supremacy_design, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(original_constitution_1787__federal_supremacy_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint centralizes decision-making power in federal structures that are responsive to national commercial interests rather than state-level majorities. Federal government can tax directly (16th Amendment era), regulate interstate commerce, enforce contracts through federal courts. The extraction is real but bounded by constitutional limits (enumerated powers, separation of powers, federalism floor of state police power). The trajectory from 0.42 to 0.65 reflects increasing federal enforcement capacity over time. Suppression (0.68): High. State nullification is suppressed through: (1) the Supremacy Clause itself, (2) the supremacy of federal treaties (enabling national foreign policy override of state objections), (3) the federal judiciary's power to invalidate state laws, (4) the explicit prohibition on state tariffs and coin money (Article I), and (5) political suppression of nullification doctrine (state legislatures face federal retaliation). Theater ratio (0.35): Relatively low. Federal supremacy is functionally enforced through real legal mechanisms (judiciary, taxation, treaty enforcement), not primarily through performative compliance. The constraint is not a Piton because it retains genuine enforcement infrastructure and the primary function (coordinating union) is real, not theatrical.
 *
 * PERSPECTIVAL GAP:
 *   State-level majorities see a Snare (subordination without exit); organized states see Tangled Rope (constrained but also coordinated); national commercial interests see Rope (pure coordination benefit); the federal government sees Tangled Rope (empowered but also checked); the analytical observer risks seeing a Mountain (structural necessity) but the beneficiary identification and suppression of nullification doctrines expose this as a false summit. The perspectival gap reveals that what appears to one perspective as an immutable requirement of union appears to another as a contingent institutional choice made by specific interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position relative to supremacy extraction. National commercial interests are beneficiaries with arbitrage options (can relocate production within federal union) — low d, negative chi. State-level majorities are victims with no exit (trapped) — high d, high chi. Organized states as actors are constrained but coordinated — moderate d, moderate chi. Federal government is institutional beneficiary but also constrained — low-to-moderate d. The engine derives these from the beneficiary/victim declarations and exit_options. The Supremacy Clause itself creates the asymmetry: it says 'this Constitution and federal laws made in pursuance are the supreme law of the land' — it speaks FOR federal interests and AGAINST state-level resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING FORM: The mandatrophy is resolved by recognizing that this is ONE reading of a contested kernel. The statement 'the 1787 Constitution's core is federal supremacy' is a reading — a choice of what to emphasize within an ambiguous text. The alternative readings (Article V, separation of powers, slavery compromises) are equally valid readings of the same text. The mandatrophy in this domain is not 'what type is the Constitution' but 'which reading of the Constitution do we adopt, and does that reading foreclose the others?' This reading (supremacy) coexists with the separation of powers reading (both are simultaneously true: the Constitution does centralize power in a federal structure AND divides it among three branches). It influences and may depend on the slavery reading (supremacy architecture served to protect slavery against abolition from northern majorities). It does not foreclose Article V (the Constitution can be amended, including its supremacy clause). Resolving the mandatrophy requires examining which reading the historical framers and ratifiers were actually committing to — a question answered by their subsequent actions and the courts' interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_supremacy_vs_state_consent,
    'Is federal supremacy a structural requirement for viable union, or a choice by framers representing specific commercial interests to centralize power?',
    'Historical comparison: confederal structures (Swiss cantons, Dutch provinces) that functioned without federal supremacy; examination of framers'' institutional alternatives and their rejection rationale; analysis of whose interests benefited from supremacy vs. alternatives',
    'If structural requirement: mountain classification justified — federal supremacy is immutable. If choice: tangled_rope justified — supremacy extracts from state majorities while providing genuine coordination benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_supremacy_vs_state_consent, empirical, 'Whether federal supremacy is structurally necessary or contingent institutional choice').

omega_variable(
    this_reading_vs_article_v,
    'Does the federal supremacy reading foreclose, coexist with, or influence the Article V amendment procedure reading?',
    'Logical analysis: Article V permits amendment of the supremacy clause itself (16th Amendment precedent). Supremacy does not logically rule out revisability. Both readings can hold in one framework: federal supremacy is the enacted rule; Article V is the rule about how to change the rule. Whether they foreclose depends on whether parties holding the supremacy reading deny that Article V is operative, or merely that it is politically immobile.',
    'If forecloses: incompatible readings, only one coherent. If coexists: both are live commitments in the system. If influences: supremacy''s centralization makes Article V amendment harder (downstream structural pressure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_vs_article_v, conceptual, 'Logical relationship between supremacy reading and Article V reading').

omega_variable(
    this_reading_vs_slavery_compromises,
    'Does the federal supremacy reading depend on or foreclose the slavery compromises reading?',
    'Causal analysis: Did supremacy architecture serve slaveholding interests? (Three-Fifths Clause amplifies slaveholding state power in federal elections; federal supremacy protects slavery against abolitionist state majorities; fugitive slave clause is federal enforcement mechanism). Did framers adopt supremacy partly to protect slavery against potential state-level abolition?',
    'If dependent: slavery reading is prerequisite to understanding federal supremacy (supremacy''s extractiveness directed at protecting slavery). If independent: both readings are structural but slavery reading is orthogonal. If forecloses: supremacy''s logic contradicts slavery''s logic (unlikely).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_vs_slavery_compromises, empirical, 'Whether federal supremacy reading depends on or is independent of slavery compromises reading').

omega_variable(
    supremacy_mechanism_equivalence,
    'Does the supremacy clause (Article VI) plus the Necessary and Proper Clause plus the federal judiciary (Article III) constitute the actual extraction mechanism, or is supremacy a framing that naturalizes structural power imbalance?',
    'Mechanism analysis: Trace enforcement pathways — how state laws are actually subordinated. Is it through: (a) actual judicial invalidation (rare, requires federal court case), (b) threat of judicial invalidation (behavioral deterrent), (c) federal pre-emption that crowds out state law, or (d) political pressure (state representatives fearing federal retaliation)? Measurement: rate of Supremacy Clause invocations over time; count of state nullification attempts vs. successful suppressions.',
    'If actual judicial mechanism: supremacy is functional. If primarily threat/political mechanism: theater ratio is higher, suggesting Tangled Rope with strong performative component. If behavioral deterrent without enforcement: extraction is through suppression, not through active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supremacy_mechanism_equivalence, empirical, 'Whether supremacy mechanism is actual legal enforcement or primarily threat/behavioral deterrent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(original_constitution_1787__federal_supremacy_design, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_fed_sup_tr_t0, original_constitution_1787__federal_supremacy_design, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cons_fed_sup_tr_t20, original_constitution_1787__federal_supremacy_design, theater_ratio, 20, 0.33).
narrative_ontology:measurement(cons_fed_sup_tr_t40, original_constitution_1787__federal_supremacy_design, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(cons_fed_sup_be_t0, original_constitution_1787__federal_supremacy_design, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_fed_sup_be_t20, original_constitution_1787__federal_supremacy_design, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cons_fed_sup_be_t40, original_constitution_1787__federal_supremacy_design, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_fed_sup_su_t0, original_constitution_1787__federal_supremacy_design, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(cons_fed_sup_su_t20, original_constitution_1787__federal_supremacy_design, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(cons_fed_sup_su_t40, original_constitution_1787__federal_supremacy_design, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(original_constitution_1787__federal_supremacy_design, enforcement_mechanism).
narrative_ontology:affects_constraint(original_constitution_1787__federal_supremacy_design, original_constitution_1787__article_v_amendment_procedure).
narrative_ontology:affects_constraint(original_constitution_1787__federal_supremacy_design, original_constitution_1787__separation_of_powers_design).
narrative_ontology:affects_constraint(original_constitution_1787__federal_supremacy_design, original_constitution_1787__slavery_compromises).

% DUAL FORMULATION NOTE:
% The 1787 Constitution generates four constraint stories, one per competing reading of the kernel. Each reading identifies a different structural core: federal supremacy (this story), Article V revisability, separation of powers, and slavery compromises. All four readings operate simultaneously in constitutional interpretation — the network edges show that each reading structurally influences the others. Federal supremacy reading influences the slavery compromises reading (supremacy architecture enabled slavery protection) and influences the separation of powers reading (federal supremacy is distributed across the three branches). Article V influences all others (all commitments are revocable, which changes their binding character). The kernel contest is not resolved by declaring one reading canonical; instead, the four stories together map the interpretive field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
