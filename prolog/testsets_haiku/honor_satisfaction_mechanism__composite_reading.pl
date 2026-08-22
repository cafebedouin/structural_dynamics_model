% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_composite, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism (Composite: State Monopoly, Bourgeois Norms, Insurance, Category-Shift)
 *   domain: social/normative/legal_history
 *
 * SUMMARY:
 *   This reading of the honor satisfaction mechanism posits that dueling's
 *   historical erosion resulted from multiple distinct, independently
 *   operating mechanisms: (1) state monopolization of legitimate violence and
 *   grievance redress, (2) bourgeois propriety norms that delegitimized
 *   honor-code-based satisfaction, (3) emergence of insurance markets that
 *   profited from the recategorization of dueling from 'legitimate honor
 *   satisfaction' to 'insurable risk' to 'criminal violence', and (4) a
 *   category-level shift where 'honor satisfaction' itself became redefined
 *   from a legitimate social need to a sign of irrationality or criminality.
 *   This reading differs from the contraction_reading (which emphasizes
 *   cognitive impossibility — dueling became unthinkable) and the
 *   decline_reading (which emphasizes gradual attrition). The composite
 *   reading asserts that multiple overlapping extractive pressures, each
 *   benefiting distinct constituencies, operated in concert to suppress a
 *   practice that lost its legitimacy not through unified causal logic but
 *   through structural recategorization. The constraint is CLAIMED as
 *   tangled_rope (genuine coordination of grievance redress WITH asymmetric
 *   extraction and active enforcement) to distinguish it from a pure snare
 *   (pure extraction with cover story) or a genuine rope (coordination
 *   without structural extraction). Extractiveness rises over the interval as
 *   the multiple mechanisms accumulate; theater rises as the state justice
 *   system increasingly performs 'neutrality' and 'rationality' while
 *   excluding honor-code institutions; suppression intensifies as enforcement
 *   infrastructure matures.
 *
 * KEY AGENTS:
 *   - state_administration: Monopolizes legitimate grievance redress; enforces the constraint through criminal law and court authority.
 *   - military_nobility: Targeted extractees; constrained exit because submission to state courts contradicts autonomous status identity.
 *   - landed_gentry: Identity-locked between honor codes (illegal, excluded) and bourgeois propriety (normatively mandatory but status-degrading).
 *   - bourgeois_professional_class: Primary beneficiaries through authority over legitimate justice and norm-setting; organizes with state to enforce the constraint.
 *   - insurance_underwriters: Emerge as new beneficiaries; profit from recategorization of dueling as insurable/calculable risk.
 *   - legal_reform_advocates: Intellectuals vindicated by suppression; consolidate authority to define rational behavior and rule of law.
 *   - dueling_code_institutions: Excluded non-agents; their function transferred to state courts; excluded from defining what counts as legitimate satisfaction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.71).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Composite: State Monopoly, Bourgeois Norms, Insurance, Category-Shift)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "social/normative/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, 'd3f24218-7c47-47b6-a94b-c1a6537cd648').
narrative_ontology:cs_kernel_codification('d3f24218-7c47-47b6-a94b-c1a6537cd648', implicit).
narrative_ontology:cs_authority_grounding('d3f24218-7c47-47b6-a94b-c1a6537cd648', extraction).
narrative_ontology:cs_interpretation_layer_present('d3f24218-7c47-47b6-a94b-c1a6537cd648').
narrative_ontology:cs_reading_relation('d3f24218-7c47-47b6-a94b-c1a6537cd648', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3f24218-7c47-47b6-a94b-c1a6537cd648', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_axiom('d3f24218-7c47-47b6-a94b-c1a6537cd648', foundational, multiple_mechanisms_necessary).
narrative_ontology:cs_axiom_status(multiple_mechanisms_necessary, holdable).
narrative_ontology:cs_axiom_grounding('d3f24218-7c47-47b6-a94b-c1a6537cd648', multiple_mechanisms_necessary, empirically_contingent).
narrative_ontology:cs_axiom('d3f24218-7c47-47b6-a94b-c1a6537cd648', foundational, honor_satisfaction_as_legitimate_social_function).
narrative_ontology:cs_axiom_status(honor_satisfaction_as_legitimate_social_function, overridden).
narrative_ontology:cs_axiom_grounding('d3f24218-7c47-47b6-a94b-c1a6537cd648', honor_satisfaction_as_legitimate_social_function, conventional).
narrative_ontology:cs_reference_frame('d3f24218-7c47-47b6-a94b-c1a6537cd648', honor_satisfaction_as_legitimate_grievance_redress).
narrative_ontology:cs_drift_state('d3f24218-7c47-47b6-a94b-c1a6537cd648', contemporary_state_monopoly_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('d3f24218-7c47-47b6-a94b-c1a6537cd648', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_administration).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_underwriters).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, legal_reform_advocates).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, military_nobility).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, landed_gentry).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, practitioners_of_dueling).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, rule_of_law_supremacy).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, state_monopoly_on_violence).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, bourgeois_propriety_norms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces prohibition on private honor satisfaction through dueling; establishes and administers the legal apparatus that substitutes state justice, damages, libel law, and reputation remedies. Consolidates monopoly on legitimate violence and on the definition of legitimate grievance redress. Extracts legitimacy and enforcement power from the displacement of private remedy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_administration, agenda_setter,
    institutional, generational, analytical, national).

% Formerly exercised private satisfaction of honor through dueling; now subject to criminal penalties for doing so. Faces reputational loss if they submit grievances to state courts (a 'degradation' from private to public remedy). Exit would mean accepting judicial authority and bourgeois-defined reputation, which contradicts their status identity as autonomous honorable actors.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, military_nobility, payer,
    powerful, biographical, constrained, national).

% Operating at the boundary of military and bourgeois worlds, dependent on honor codes for status in rural hierarchies. Dueling enforcement networks once provided them grievance recourse; criminalization forces them toward state courts or social exclusion. Identity as 'gentleman' becomes unstable when honor satisfaction is legally prohibited.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, landed_gentry, payer,
    powerful, biographical, identity_locked, regional).

% Face criminal prosecution, reputation loss in both dueling subculture and broader society, and loss of the cultural infrastructure that once legitimated their practice. Trapped between suppressed dueling subculture and compulsory entry into state legal system; neither route preserves their prior status.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, practitioners_of_dueling, payer,
    moderate, biographical, trapped, national).

% Benefits from state monopoly on legitimate justice; dueling suppression establishes bourgeois norms (propriety, legal recourse, reputation through civil courts) as the standard for all classes. Provides expertise in law, damages calculation, insurance, and reputation management; their status rises as state administration outsources these functions.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class, agenda_setter).

% Emerge as dueling becomes uninsurable and replaced by legal liability. Develop markets for professional liability, reputation insurance, and damages-claim financing. Dueling prohibition creates demand for their new product categories; they extract rent on the recategorization.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_underwriters, beneficiary,
    organized, biographical, mobile, national).

% Intellectuals, jurists, and reformers who championed abolition of dueling as inconsistent with rule of law and rational justice. Vindication of their position through suppression consolidates their authority to define what counts as legitimate grievance redress and rational behavior.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_reform_advocates, beneficiary,
    moderate, generational, mobile, national).

% Formal and informal networks (dueling codes, honor societies, second-broker networks) that once structured satisfaction mechanisms. Criminalization and suppression exclude them from legal recognition; their function is transferred to state courts and bourgeois norm enforcement.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, dueling_code_institutions, excluded,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_non_agent(honor_satisfaction_mechanism__composite_reading, dueling_code_institutions).

% Examines how multiple independent mechanisms (state monopoly enforcement, bourgeois propriety norms, insurance market creation, category redefinition from 'honor satisfaction' to 'criminal violence') operated simultaneously to erode dueling, rather than through a single causal pathway.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, state_administration).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, state-administered system for grievance redress and reputation management in place of fragmented, extra-legal honor satisfaction. Consolidates the definition of legitimate grievance, legitimate remedy, and legitimate status claims under state law and bourgeois propriety norms.
% TRANSFER_FUNCTION: Transfers legitimate authority over honor disputes from military nobility and dueling practitioners to state administration, legal professionals, and bourgeois norm-setters. Extracts deference to state law and bourgeois definitions of propriety from classes formerly autonomous in their honor satisfaction. Transfers economic value from dueling-related activities (second-brokering, honor code administration) to state courts and insurance underwriters.
% ABSENT_VOICES: Dueling practitioners and honor-code institutions are excluded from the legal process that criminalized them; they could testify that honor satisfaction through private contest is structurally necessary for certain status hierarchies, that state courts do not provide equivalent satisfaction, and that criminalization constitutes category redefinition masquerading as prohibition. They are not in the room where the constraint is defined.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, honor satisfaction would resume within days in military and gentry circles; dueling codes would re-activate; reputation management would fragment again; insurance markets for liability would collapse; state justice monopoly would weaken; and bourgeois propriety norms would lose their enforcement apparatus. The social order that the constraint holds in place would reorganize around honor codes within weeks.
% FOUNDING_PROBLEM: Private satisfaction of honor disputes through dueling produced unpredictable violence, legal chaos when duelists crossed state lines, and left grievances outside state control. The founding problem is framed as disorder and irrationality; dueling practitioners framed it as the only available satisfaction for honor disputes that state law did not recognize.
% FOUNDING_PROBLEM_CORROBORATION: The state administration, legal reformers, and insurance advocates testify the problem was disorder and rule-of-law violation. Military nobility and dueling practitioners testify the founding problem was inadequacy of state courts for honor disputes and the imposition of bourgeois propriety on classes with different status systems. Historical testimony from both seats is on record; neither has conceded the other's framing.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The composite reading attends to extractiveness rising from 0.35 to 0.68 over 100 time units. Early in the interval (t0-t30), extractiveness is moderate because dueling suppression is nascent — enforcement machinery is incomplete, dueling codes persist in practice, and the state apparatus lacks full authority. Mid-interval (t30-t50), extractiveness rises sharply as the state monopoly solidifies, bourgeois norms are institutionalized through schools and professions, insurance markets open, and the category shift ('honor satisfaction' → 'criminal behavior') gains intellectual authority. Late interval (t75-t100), extractiveness plateaus near 0.68 because the constraint has fully transformed the landscape — dueling is culturally dead, state courts are hegemonic, and the mechanisms require only periodic enforcement rather than active suppression of viable alternatives. Suppression mirrors this trajectory: initially moderate (0.42) because alternatives exist and enforcement is inconsistent, rising steeply (t30-t50) as enforcement machinery hardens, then plateauing (0.71) as the behavioral change becomes internalized. Theater (0.28→0.52) rises throughout because the state increasingly performs 'neutral justice' and 'rational law' while actively excluding the dueling code institutions that could challenge its legitimacy. The temporal pattern is NOT monotonic decline (the decline_reading would predict), nor a sudden cognitive impossibility (the contraction_reading), but rather a composite of overlapping extractive mechanisms whose effects accumulate and then stabilize once the category shift is complete.
 *
 * PERSPECTIVAL GAP:
 *   The state administration and bourgeois beneficiaries compute this as rope: genuine coordination (unified grievance redress replacing fragmented honor codes) WITH asymmetric extraction (state authority over what counts as legitimate satisfaction). The military nobility and dueling practitioners compute it as snare: the coordination function is window-dressing for exclusion and forced conformity to bourgeois norms. From the beneficiary seats, extraction is a cost of coordination; from the target seats, extraction is the entire point and coordination is the cover story. The engine computes per-seat type from power/exit; the structural asymmetry (powerful beneficiaries with mobile exit, powerful targets with constrained/identity-locked exit) should produce divergent classifications across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State administration: d ≈ 0.0 (beneficiary seat; collects monopoly authority; analytical exit). Bourgeois professional class: d ≈ 0.2 (beneficiary; organized power; mobile exit). Insurance underwriters: d ≈ 0.15 (beneficiary; organized power; profit from recategorization; mobile exit). Military nobility: d ≈ 0.85 (target; powerful formally but constrained exit because submission to state law contradicts status identity; high extraction). Landed gentry: d ≈ 0.80 (target; identity-locked because honor-code identity is incompatible with bourgeois propriety; high extraction). Practitioners of dueling: d ≈ 0.90 (target; moderate power but trapped exit; lose both subculture AND status route). The beneficiary seats all have moderate-to-low d because they extract value without bearing suppression cost; the target seats have high d because they both bear suppression AND are excluded from defining the legitimate remedy. No directionality_overrides are necessary; the derivation from beneficiary/victim + exit should produce these values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (private honor satisfaction creates disorder and violence) is classified as 'contested' because dueling practitioners and military nobility disputed the diagnosis: they argued that the founding problem was the state's inadequacy at recognizing honor disputes, not the existence of dueling. The suppression of dueling does not resolve this dispute; it forecloses debate by excluding the contesting parties from legitimate grievance voice. The mandatrophy risk is that the constraint persists as enforcement inertia even if the founding problem has been solved (state courts now provide adequate remedy for non-honor grievances) and even if the 'disorder' was never the core issue (it was authority and norm supremacy). The rising theater_ratio (0.28→0.52) suggests that the state justice system increasingly performs 'rational, neutral justice' while the actual work of suppression has shifted from active enforcement (t30: suppression_requirement 0.68) to internalization and category control. The theater pattern signals a piton-ward drift: the constraint's coordination function may be genuine, but the machinery devoted to suppressing alternatives (excluding dueling codes, prohibiting satisfaction through contest, redefining honor disputes as criminal matters) is increasingly ceremonial and self-perpetuating rather than directed at a live coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_monopoly_extraction_vs_coordination,
    'Does the state''s monopoly on legitimate grievance redress constitute essential coordination infrastructure, or is it primarily extractive authority over what counts as legitimate?',
    'Comparative analysis of non-state honor satisfaction systems that coexist with state law (e.g., contemporary honor courts in some diaspora communities, professional mediation networks). If coordination functions persist outside state apparatus, the state claim to necessity is weaker.',
    'If the monopoly is primarily extractive, the constraint is closer to snare than tangled_rope; if it provides genuine coordination unavailable elsewhere, the classification holds at tangled_rope. This is the core distinction between this reading and the contraction_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_extraction_vs_coordination, empirical, 'Whether state monopoly on grievance redress is functionally necessary or primarily extractive authority.').

omega_variable(
    bourgeois_normativity_internalization,
    'Is the decline of dueling primarily due to internalization of bourgeois propriety norms (targets came to believe dueling was irrational), or due to suppression machinery (targets were excluded from practicing despite continued belief in its legitimacy)?',
    'Archival evidence from dueling practitioners'' own writings, diaries, and justifications. Do they shift toward bourgeois rationality frames, or do they maintain honor-code justifications while covertly practicing or reluctantly abandoning?',
    'If primarily internalized, the suppression is lower than authored and the theater_ratio is lower (targets actively adopt the new norms). If primarily suppressed despite maintained belief, suppression is higher and theater is higher (enforcement without conversion). The answer affects the direction of late-interval dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bourgeois_normativity_internalization, empirical, 'Whether targets internalized bourgeois norms or were suppressed despite maintaining belief in dueling legitimacy.').

omega_variable(
    insurance_recategorization_primacy,
    'Did insurance underwriters drive the recategorization of dueling (from ''honor satisfaction'' to ''insurable risk'' to ''criminal violence''), or did they opportunistically profit from a recategorization driven by state and bourgeois forces?',
    'Historical record of insurance industry advocacy for dueling suppression, comparative analysis of timing (when did insurance markets emerge relative to state prohibition, relative to bourgeois reform discourse), and economic analysis of dueling-related insurance product emergence.',
    'If insurance drove recategorization, the extractive mechanism is more conscious and coordinated, and gains distributed more asymmetrically. If opportunistic, the recategorization is more distributed across state, bourgeois, and market actors, and no single constituency ''designed'' the constraint. This affects assignment of agenda_setter role.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurance_recategorization_primacy, empirical, 'Whether insurance markets drove or opportunistically profited from dueling suppression and recategorization.').

omega_variable(
    composite_vs_single_mechanism_causation,
    'Is this reading''s claim (multiple independent mechanisms) structurally true, or does one mechanism (state monopoly, or bourgeois normativity, or category shift) sufficiently explain the outcome with others as secondary effects?',
    'Counterfactual historical analysis: if state prohibition had succeeded without bourgeois norm change, would dueling have persisted in practice? If bourgeois norms had shifted without state enforcement, would dueling have survived? If category shift had occurred without enforcement, would practice have continued? Natural experiments in polities with varying combinations of these mechanisms.',
    'If a single mechanism is sufficient, the constraint is more clearly driven by one beneficiary constituency (the state, the bourgeoisie, or the intellectuals) and the tangled_rope classification may shift toward snare if that constituency is concentrated. If genuinely composite, the constraint''s extractiveness is distributed and the tangled_rope classification holds as the interdependence of mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_vs_single_mechanism_causation, conceptual, 'Whether the constraint''s erosion was driven by multiple independent mechanisms or by one sufficient mechanism with secondary effects.').

omega_variable(
    kernel_reading_disambiguation,
    'Which of the three sibling readings (composite_reading, contraction_reading, decline_reading) best captures the historical causes of dueling''s erosion, and are the readings mutually exclusive or do multiple readings apply to different phases or populations?',
    'Narrative historical analysis distinguishing: (1) when and among whom dueling became cognitively impossible (contraction); (2) when dueling declined in frequency but remained practiced (decline); (3) when multiple mechanisms operated in parallel (composite). Possible answer: all three operate at different levels — contraction among educated urban bourgeoisie, decline among military nobility, composite mechanisms overall.',
    'If all three readings describe distinct populations or phases, the kernel contains irreducible heterogeneity and no single reading fully captures the constraint. This affects which reading is canonical and how the constraint''s classification integrates across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the composite, contraction, and decline readings describe mutually exclusive causes or different aspects of a heterogeneous historical process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__composite_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t15, honor_satisfaction_mechanism__composite_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(hono_tr_t15, observed).
narrative_ontology:measurement(hono_tr_t30, honor_satisfaction_mechanism__composite_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(hono_tr_t30, observed).
narrative_ontology:measurement(hono_tr_t50, honor_satisfaction_mechanism__composite_reading, theater_ratio, 50, 0.51).
narrative_ontology:measurement_basis(hono_tr_t50, observed).
narrative_ontology:measurement(hono_tr_t75, honor_satisfaction_mechanism__composite_reading, theater_ratio, 75, 0.52).
narrative_ontology:measurement_basis(hono_tr_t75, observed).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_mechanism__composite_reading, theater_ratio, 100, 0.52).
narrative_ontology:measurement_basis(hono_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t15, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(hono_be_t15, observed).
narrative_ontology:measurement(hono_be_t30, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(hono_be_t30, observed).
narrative_ontology:measurement(hono_be_t50, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(hono_be_t50, observed).
narrative_ontology:measurement(hono_be_t75, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 75, 0.67).
narrative_ontology:measurement_basis(hono_be_t75, observed).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement_basis(hono_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t15, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(hono_su_t15, observed).
narrative_ontology:measurement(hono_su_t30, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(hono_su_t30, observed).
narrative_ontology:measurement(hono_su_t50, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(hono_su_t50, observed).
narrative_ontology:measurement(hono_su_t75, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 75, 0.71).
narrative_ontology:measurement_basis(hono_su_t75, observed).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 100, 0.71).
narrative_ontology:measurement_basis(hono_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, bourgeois_propriety_norm_system).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, insurance_market_liability_categorization).

% DUAL FORMULATION NOTE:
% This is one reading of the honor satisfaction mechanism kernel. It asserts that dueling's historical erosion resulted from multiple distinct, overlapping extractive mechanisms (state monopoly, bourgeois norms, insurance recategorization, category shift) operating in parallel, rather than from a single causal pathway. Sibling readings (contraction_reading, decline_reading) instantiate different causal theories of the same kernel: was dueling cognitively impossible, gradually attenuated, or multiply suppressed? The three readings coexist as live historical interpretations; none forecloses the others within a single analytic framework, though they influence each other (a demonstrated contraction among some populations lends credibility to the composite reading's account of multiple mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
