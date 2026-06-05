% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading: Unilateral Parliamentary Authority Over Aotearoa/New Zealand
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Crown Sovereignty Reading interprets the Treaty of Waitangi (1840) as
 *   establishing Westminster parliamentary supremacy over Aotearoa New
 *   Zealand with no requirement for ongoing Māori consent to legislation or
 *   resource allocation. Under this reading, the Crown's authority is
 *   plenary: Parliament can legislate unilaterally on all matters affecting
 *   Māori interests, Māori governance is subordinate to Crown authority, and
 *   indigenous rights exist only insofar as Parliament grants them. This
 *   constraint has been the operative legal doctrine since 1840 and remains
 *   the formal constitutional position. However, its structural properties
 *   reveal it as a **snare with false summit characteristics**: beneficiaries
 *   (Crown executive, settler Parliament) present parliamentary sovereignty
 *   as an immutable law of Westminster governance, naturalizing what is
 *   actually a contingent institutional choice that serves specific
 *   interests. The measurements show extraction increasing from 1840 (0.58)
 *   to 1990 (0.70) as Crown capacity to enforce unilateral authority
 *   expanded, followed by slight decline (0.68 by 2020) due to settlement
 *   mechanisms and treaty jurisprudence. Theater ratio rising from 0.35 to
 *   0.58 indicates the doctrine increasingly relies on reaffirming its own
 *   supremacy rather than functional enforcement — a signature piton
 *   degradation pattern. The constraint cannot be properly classified without
 *   recognizing it as one reading of a contested kernel; the partnership and
 *   rangatiratanga readings offer structurally different allocations of the
 *   same sovereignty space.
 *
 * KEY AGENTS:
 *   - Crown Executive: Primary beneficiary (institutional/arbitrage) — unilateral legislative authority, resource control, veto power over Māori claims
 *   - Settler Parliament: Primary beneficiary (institutional/arbitrage) — ability to legislate without Māori consent, normalization of Crown interests as 'public interest'
 *   - Māori Iwi Collectives: Primary victim (powerless/trapped) — governance authority subordinated, resources allocated without consent, cultural recognition contingent on Crown approval
 *   - Indigenous Governance Authority: Victim (powerless/trapped) — structural authority eliminated; tribe-level self-determination constrained by unilateral Crown override
 *   - Waitangi Tribunal: Secondary actor (organized/constrained) — investigates treaty claims but lacks enforcement power; tribunal recommendations may be ignored by Crown
 *   - Bill of Rights Act 1990 / Human Rights Act 1993: Partial constraint on Crown power but does not modify parliamentary sovereignty doctrine — read as Scaffold (temporary constraint) rather than fundamental reform
 *   - Analytical Observer: Observes the false summit — the doctrine is presented as natural law but serves identifiable beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.72).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.68).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, snare).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading: Unilateral Parliamentary Authority Over Aotearoa/New Zealand").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'f2ea6d24-ffa6-4ec1-b001-809b7d4fa406').
narrative_ontology:cs_kernel_codification('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', fixed_text).
narrative_ontology:cs_authority_grounding('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', extraction).
narrative_ontology:cs_interpretation_layer_present('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406').
narrative_ontology:cs_reading_relation('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', waitangi_sovereignty_allocation__partnership_reading, forecloses).
narrative_ontology:cs_reading_relation('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', foundational, parliament_supremacy_absolute).
narrative_ontology:cs_axiom_status(parliament_supremacy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', parliament_supremacy_absolute, conventional).
narrative_ontology:cs_axiom('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', foundational, waitangi_subordinate_to_crown_interpretation).
narrative_ontology:cs_axiom_status(waitangi_subordinate_to_crown_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', waitangi_subordinate_to_crown_interpretation, conventional).
narrative_ontology:cs_reference_frame('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', crown_plenary_sovereignty).
narrative_ontology:cs_drift_state('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', contemporary_treaty_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f2ea6d24-ffa6-4ec1-b001-809b7d4fa406', '2026-02-26T14:23:45Z').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_parliament).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_collectives).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, indigenous_governance_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MĀORI IWI AUTHORITY (SNARE) — Trapped within a constitutional framework that unilaterally subordinates tribal governance to Westminster parliamentary will. No institutional exit mechanism; governance authority can be overridden by statute at any moment without consent or negotiation. Trapped exit reflects both legal doctrine (parliamentary supremacy) and practical capacity constraints (resource asymmetry, institutional isolation). Experiences maximum extraction: authority stripped, resources allocated unilaterally, cultural recognition contingent on Crown approval.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__crown_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CROWN EXECUTIVE AND SETTLER PARLIAMENT (ROPE) — Experiences the sovereignty constraint as pure coordination: the doctrine establishes clear decision-making authority (Parliament's right to legislate without Māori veto) and enables efficient resource allocation (Crown retains unilateral control of land, resources, policy). Benefits from the constraint through institutional stability, enforcement authority, and resource capture. Arbitrage exit: Crown can ignore the constraint only by formally amending the Constitution, which Parliament controls absolutely.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__crown_sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED MĀORI RIGHTS MOVEMENTS (TANGLED ROPE) — Constrained by legal doctrine and resource barriers, but possess organizational capacity and partial institutional access (Waitangi Tribunal, settlement processes, parliamentary representation). Experience the constraint as mixed: unilateral Crown power extracts governance authority and resources, but settlement mechanisms and tribunal recommendations provide limited coordination function. Extraction is substantial but not total — organized pressure has shifted policy incrementally. Constrained exit reflects high costs to rupture the constitutional framework but genuine capacity to negotiate within it.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (FALSE SUMMIT MOUNTAIN) — From a civilizational/universal perspective, parliamentary sovereignty appears as an immutable feature of Westminster constitutional law: an irreducible structural principle that cannot be revised without dissolving the entire system. The doctrine is presented as a natural law of parliamentary governance. However, the structural data contradicts this: the constraint has clear beneficiaries (Crown, settler Parliament) whose institutional power depends on maintaining the sovereignty doctrine. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement. Parliamentary sovereignty is a **chosen** allocation mechanism that serves specific interests, not a law of nature.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__crown_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__crown_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__crown_sovereignty_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): High. The Crown captures unilateral decision-making authority, resource allocation, and veto power over Māori governance. The extraction is asymmetric: Māori iwi have no equivalent power. The 0.72 value reflects that this extraction is substantial and durable, though partially mitigated by settlement mechanisms and Waitangi Tribunal influence (which have incrementally constrained pure Crown unilateralism). Initial extraction in 1840 was lower (0.58) because early Crown authority was more contested and less administratively developed; peak extraction occurs around 1940-1990 when the Crown's institutional capacity was maximum and settlement mechanisms had not yet developed. Slight decline by 2020 reflects settlement processes and treaty jurisprudence creating marginal constraints. Suppression (0.68): High. Maintained through multiple mechanisms: (1) legal doctrine (parliamentary supremacy articulated as immutable law), (2) institutional barriers (Māori lack formal legislative veto or constitutional protection), (3) resource asymmetry (Crown controls enforcement apparatus, police, military), (4) historical trauma and ongoing marginalization constraining Māori organizational capacity. Theater ratio (0.55): Moderate. Initially low (0.35 in 1840) when Crown authority was actively imposed through direct coercion and administrative assertion. Increases over time (0.58 by 2020) because the doctrine increasingly relies on performative reaffirmation (constitutional scholars, judicial opinions, political rhetoric naturalizing the doctrine) rather than active enforcement. The constraint no longer requires constant coercion — it is maintained through institutional inertia and the false summit framing (parliamentary sovereignty as natural law). This rising theater is a piton signature: the doctrine persists not because it functions effectively but because alternatives have not displaced it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates perspectival divergence across all four dimensions. The Crown and settler Parliament see Rope: the doctrine establishes clear authority and enables efficient coordination of Crown resource allocation. Organized Māori movements see Tangled Rope: the constraint extracts governance authority but settlement mechanisms provide limited coordination function and partial access to Crown decision-making. Trapped iwi see Snare: unilateral extraction with no exit mechanism. The analytical observer risks seeing Mountain (parliamentary sovereignty as immutable law) but structural data reveals this as a false summit: beneficiaries exist, the doctrine serves their interests, and it has been contingently chosen rather than naturally imposed. The perspectival gap is maximized between the beneficiary (institutional/arbitrage) view and the victim (powerless/trapped) view — chi values diverge sharply due to directionality. The Tangled Rope perspective (organized actors) demonstrates that agency and coalition pressure can incrementally reduce extraction, though not eliminate it within the Crown sovereignty reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective derive from structural position: (1) Crown/Parliament beneficiary with arbitrage exit: d ≈ 0.05, f(d) ≈ -0.12, chi ≈ ε × (-0.12) × 1.0 ≈ negative (beneficiary sees low extraction or coordination benefit). (2) Māori iwi victim with trapped exit: d ≈ 0.95, f(d) ≈ 1.42, chi ≈ ε × 1.42 × 1.0 ≈ high extraction (target experiences maximum extraction). (3) Organized movements victim with constrained exit: d ≈ 0.70, f(d) ≈ 1.05, chi ≈ ε × 1.05 × 1.0 ≈ moderate-high extraction (organized pressure reduces experienced extraction below trapped level). (4) Analytical observer: d ≈ 0.72, f(d) ≈ 1.15, chi ≈ ε × 1.15 × 1.0 ≈ high analytical-view extraction (the false summit reveals a snare). Directionality is not overridden in this story — the beneficiary/victim declarations and exit options produce accurate d values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint classification is Snare (ε = 0.72 > 0.46, suppression = 0.68 ≥ 0.60, χ ≥ 0.66 from powerless victim perspective). The mandatrophy is resolved by recognizing that the doctrine's false summit framing (naturalizing parliament sovereignty as immutable law) IS part of the snare mechanism. The snare is maintained not just through coercion but through epistemically capturing all observers into naturalizing it. The mountain view (perspective 4) is itself a signature of successful snare operation — beneficiaries benefit from having their constraint naturalized. The analytical observer's task is to expose this: the constraint is not a law of nature, it is a snare dressed as doctrine. The Tangled Rope perspective (organized movements) shows that incrementally reducing theater and suppression can degrade the snare toward rope-like dynamics, but within the Crown sovereignty reading framework, the snare structure is fundamental: one party retains unilateral authority while the other bears costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_sovereignty_contingency,
    'Is parliamentary sovereignty an immutable feature of Westminster systems or a contingent institutional choice that serves specific beneficiaries?',
    'Historical analysis of Westminster jurisdictions that have formally modified or constrained parliamentary supremacy (e.g., South Africa''s constitutional court authority, Canada''s Charter supremacy, New Zealand''s own Bill of Rights Act 1990 and Human Rights Act 1993 establishing judicially enforceable limits on legislation). Comparison of governance outcomes in systems with absolute parliamentary sovereignty vs. constitutional constraints.',
    'If contingent: the mountain classification is a false summit; the constraint is a snare maintained by beneficiary doctrine. Parliamentary supremacy becomes a political choice subject to renegotiation. If immutable: parliamentary supremacy is a genuine natural law of Westminster systems; reform would require abandoning Westminster entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_sovereignty_contingency, empirical, 'Whether parliamentary sovereignty is immutable or contingent institutional choice').

omega_variable(
    waitangi_treaty_supremacy_interpretation,
    'Does the Treaty of Waitangi (1840) establish a framework that coexists with or forecloses Crown unilateral sovereignty?',
    'Treaty text analysis (English vs Māori versions and their material differences); historical record of Crown and iwi understanding at 1840; evolution of judicial interpretation (Waitangi Tribunal jurisprudence, Court of Appeal decisions on treaty principles); comparison of treaty authority with constitutional doctrine across different Commonwealth jurisdictions.',
    'If Waitangi coexists with Crown sovereignty: the partnership reading and rangatiratanga reading are constraining but not foreclosing; multiple readings can be held simultaneously. If Waitangi forecloses unilateral Crown sovereignty: the Crown reading and partnership reading logically contradict each other within a single committed framework; the constraint cannot be Rope (coordination) — it must be Snare (extraction under the guise of legal doctrine).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waitangi_treaty_supremacy_interpretation, conceptual, 'Whether Waitangi Treaty coexists with or forecloses Crown unilateral sovereignty').

omega_variable(
    doctrine_vs_practice_divergence,
    'How much does actual Crown governance practice diverge from the doctrine of absolute parliamentary sovereignty? Is the doctrine still functionally enforced or is it maintained through theater?',
    'Analysis of legislative history: frequency of Crown departing from strict parliamentary supremacy doctrine in practice (consultation with iwi, accommodation of treaty claims, deference to Waitangi Tribunal recommendations, application of the Bill of Rights Act 1990). Measurement of theater ratio: percentage of sovereignty assertions that are performative (reaffirming doctrine) vs. functional (actually overriding Māori authority). Interviews with institutional actors on whether parliamentary supremacy is actively enforced or tacitly negotiated.',
    'If high divergence and high theater: the constraint is degenerating toward Piton (inertial, performative). If high divergence and low theater: actual practice is already reading toward partnership or rangatiratanga; doctrine lags lived reality. If low divergence and high enforcement: the snare is actively maintained and functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_practice_divergence, empirical, 'Divergence between parliamentary sovereignty doctrine and actual Crown governance practice').

omega_variable(
    false_summit_natural_law_claim,
    'Is parliamentary sovereignty presented as a natural law of Westminster governance to obscure that it is a contingent institutional choice that benefits the Crown and settler institutions?',
    'Discourse analysis of constitutional scholarship, judicial opinions, and political rhetoric: how often is parliamentary sovereignty framed as ''immutable'', ''inherent to Westminster'', ''foundational'', vs. framed as ''chosen'', ''subject to amendment'', ''historically contingent''. Analysis of counter-examples (jurisdictions that constrain parliamentary sovereignty) and whether they are acknowledged or suppressed in New Zealand constitutional discourse. Examination of whether the mountain reading serves institutional interests (Crown legitimation, settler security, doctrine preservation).',
    'If the false summit framing is intentional/systematic: the doctrine serves beneficiary interests through naturalization. The constraint is snare maintained by epistemically capturing its own observers. If accidental/residual: the doctrine is genuinely believed to be natural law, but beneficiaries benefit regardless. The power of the false summit is that beneficiaries need not intend the naturalization for it to work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether parliamentary sovereignty doctrine naturalizes contingent institutional choice').

omega_variable(
    rangatiratanga_structural_incompatibility,
    'Does the Crown sovereignty reading structurally foreclose Māori rangatiratanga (chiefly authority), or can both exist as coexisting readings of the same Treaty kernel?',
    'Treaty text analysis and jurisprudence: examination of whether ''rangatiratanga'' (in the Māori text, Article Two) is read as absolute tribal authority (foreclosing Crown unilateral power) or as conditional/delegated authority (coexisting with Crown sovereignty). Historical understanding at 1840 of what rangatiratanga meant to iwi signatories vs. what the Crown claimed it meant. Waitangi Tribunal findings on whether rangatiratanga is exercisable independent of Crown consent.',
    'If foreclosing: Crown sovereignty and rangatiratanga are logically contradictory; only one can be held in a single framework. If coexisting: both can be live readings; the conflict is political rather than logical. If influences (asymmetrically): Crown sovereignty constrains rangatiratanga''s exercise but does not rule it out — iwi retain some autonomy within Crown parameters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rangatiratanga_structural_incompatibility, conceptual, 'Whether Crown sovereignty structurally forecloses or coexists with Māori rangatiratanga').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waitangi_crown_theater_1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(waitangi_crown_theater_1890, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(waitangi_crown_theater_1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(waitangi_crown_theater_1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 150, 0.55).
narrative_ontology:measurement(waitangi_crown_theater_2020, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 180, 0.58).

% Extraction over time
narrative_ontology:measurement(waitangi_crown_extractiveness_1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(waitangi_crown_extractiveness_1890, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(waitangi_crown_extractiveness_1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 100, 0.72).
narrative_ontology:measurement(waitangi_crown_extractiveness_1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 150, 0.7).
narrative_ontology:measurement(waitangi_crown_extractiveness_2020, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 180, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(waitangi_crown_suppression_1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(waitangi_crown_suppression_1890, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% The Waitangi Treaty has generated multiple structurally distinct constraints corresponding to different readings of the kernel. The Crown sovereignty reading (this file) models unilateral Crown authority and generates Snare classification (ε=0.72). The partnership reading (sibling) models bilateral governance and generates Tangled Rope (ε≈0.45-0.55). The rangatiratanga reading (sibling) models independent Māori authority and generates Rope or Mountain (ε≈0.08-0.30 depending on whether it is read as constitutive or aspirational). These are NOT the same constraint viewed from different angles — they have different ε values, different beneficiary/victim sets, and different functional forms. Each reading is a separate constraint story. The network linkage shows how readings compete: if rangatiratanga is established in law, the Crown reading's authority is foreclosed; if partnership is institutionalized, the Crown reading becomes coercive rather than coordinative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
