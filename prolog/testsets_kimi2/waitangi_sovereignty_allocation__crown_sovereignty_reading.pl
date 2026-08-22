% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of Treaty of Waitangi Article I
 *   domain: constitutional law / indigenous rights / post-colonial governance
 *
 * SUMMARY:
 *   This constraint instantiates the Crown sovereignty reading of the Treaty
 *   of Waitangi: English Article I is read as a complete cession of
 *   sovereignty to the Crown, establishing Westminster parliamentary
 *   supremacy over New Zealand. Under this reading, the Crown and its
 *   parliamentary successors possess plenary legislative power without
 *   requiring MÄori consent, enabling unilateral resource allocation and the
 *   legal subordination of MÄori interests to parliamentary will. The kernel
 *   (waitangi_sovereignty_allocation) is contested: two sibling readings
 *   (partnership and rangatiratanga) derive structurally distinct constraints
 *   from the same Treaty text. This file isolates the Crown sovereignty
 *   reading as a single Îµ-invariant constraint.
 *
 * KEY AGENTS:
 *   - NZ Crown and Parliament: Primary agenda-setter (institutional/constrained) â claims and exercises plenary supremacy.
 *   - MÄori iwi and hapÅ«: Primary payer (organized/identity_locked) â bear extraction through subordinated jurisdiction and dispossession.
 *   - NZ Judiciary: Secondary agenda-setter (institutional/constrained) â enforces supremacy doctrine via precedent.
 *   - Settler citizenry: Beneficiary (organized/mobile) â receives coordination benefits of parliamentary governance without bearing subordination costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.82).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of Treaty of Waitangi Article I").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional law / indigenous rights / post-colonial governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5').
narrative_ontology:cs_kernel_codification('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', fixed_text).
narrative_ontology:cs_authority_grounding('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', lineage).
narrative_ontology:cs_interpretation_layer_present('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5').
narrative_ontology:cs_reading_relation('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', foundational, english_article_i_complete_cession).
narrative_ontology:cs_axiom_status(english_article_i_complete_cession, holdable).
narrative_ontology:cs_axiom_grounding('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', english_article_i_complete_cession, empirically_contingent).
narrative_ontology:cs_axiom('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', foundational, parliamentary_supremacy_unlimited).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_unlimited, holdable).
narrative_ontology:cs_axiom_grounding('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', parliamentary_supremacy_unlimited, conventional).
narrative_ontology:cs_reference_frame('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', westminster_supremacy_absolute).
narrative_ontology:cs_drift_state('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', contemporary_treaty_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3eb9bb8d-ab6a-4088-b357-a0e38db6e7a5', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, nz_crown_parliament).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_citizenry).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and exercises plenary legislative power over New Zealand territory by interpreting English Article I as a complete cession of sovereignty. Sets law unilaterally, allocates resources, and overrides or ignores MÄori consent requirements while maintaining that parliamentary supremacy is the constitutional foundation of the state.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, nz_crown_parliament, agenda_setter,
    institutional, generational, constrained, national).

% Subject to parliamentary legislation governing lands, fisheries, forests, and taonga. Treaty claims are heard by the Waitangi Tribunal but recommendations are non-binding. Structural subordination of rangatiratanga to Crown law is enforced through courts, resource consents, and criminal jurisdiction. Exit from the Crown legal order would require severing identity from whakapapa and whenua.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    organized, generational, identity_locked, national).

% Interprets the English text of Article I as ceding complete sovereignty and upholds the doctrine of parliamentary supremacy against Treaty-based challenges. Bound by precedent and the constitutional convention that Parliament is sovereign; cannot recognize MÄori jurisdictional authority without legislative authorization.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, nz_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Receives coordinated governance, property rights, democratic representation, and public order through the Westminster parliamentary system. Benefits from state infrastructure and legal certainty without bearing the subordination or dispossession costs imposed on MÄori.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_citizenry, beneficiary,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, nz_crown_parliament).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single central legislature with plenary authority over New Zealand territory, replacing fragmented governance with a unified parliamentary legal order capable of securing settler property, administering justice, and enabling imperial and later national governance.
% TRANSFER_FUNCTION: Transfers sovereign authority, land allocation power, and jurisdictional supremacy from MÄori chiefs and communities to the Crown and its parliamentary successors; imposes subordination costs on MÄori in the form of diminished jurisdiction, confiscation risk, and unilateral resource allocation.
% ABSENT_VOICES: MÄori signatories who understood the MÄori text retained tino rangatiratanga; rangatira not present at signings; and contemporary MÄori constitutional advocates who assert that English Article I did not authorize plenary supremacy. These voices are structurally excluded from the authoritative interpretive framework that declares sovereignty fully ceded.
% DISAPPEARANCE_RATIONALE: If the Crown sovereignty reading vanishedâif parliamentary supremacy were recognized as legally limited by MÄori authorityâthe New Zealand constitutional order would reorganize. Land titles, resource consents, and criminal jurisdiction would require dual or shared legitimacy; Westminster-style supremacy would collapse in favor of a bi-national or confederal arrangement.
% FOUNDING_PROBLEM: The absence of a unified legal authority over New Zealand territory capable of securing settler property, preventing inter-tribal conflict, and enabling British imperial governance.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial constitutional scholars and international human rights bodies (e.g., UN Special Rapporteur on Indigenous Rights) attest that the imperial governance vacuum is resolved and the current arrangement persists as structural discrimination. The Crown and its legal officers assert the problem of public order remains live. No sovereign arbiter accepted by both parties exists.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the reading enables unilateral legislative and resource-allocation authority over MÄori without consent. Suppression (0.78) is high because the persistence of the constraint depends on active judicial and legislative exclusion of rangatiratanga-based alternatives. Theater ratio (0.35) reflects moderate performative maintenance of sovereignty myths (e.g., ceremonial deference to Treaty principles that lack legal bite) alongside genuine state-coordination functions. Accessibility collapse (0.72) is substantial because MÄori legal alternatives are marginalized but not fully erased. Resistance (0.68) reflects sustained MÄori political mobilization, Tribunal claims, and direct action. The measurement series share a single time grid (0â184) to prevent temporal misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Crown/Parliament) experiences the constraint as necessary constitutional order and legitimate governance. The payer seat (MÄori) experiences the same structure as colonial extraction and jurisdictional dispossession. The engine computes this divergence from structural data; the authored claim (tangled_rope) asserts that both perceptions are structurally groundedâthere is genuine coordination and genuine asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The NZ Crown/Parliament is the structural beneficiary (low d): it collects sovereign authority and resource-allocation power. Settler citizenry sits near the beneficiary end (low-moderate d) through genuine coordination benefits. MÄori iwi/hapÅ« are the structural target (high d): they bear extraction, have identity-locked exit, and face amplified effective extraction due to their organized-but-subordinated power and national scope. The judiciary sits betweenâconstrained by doctrine, it enforces extraction without directly capturing the gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (absence of unified authority in 1840) is dead: New Zealand has a functioning state. Yet the arrangement persists with high extraction and active enforcement. The R5 genealogy interview records founding_problem_status=dead and disappearance_verdict=world_rearranges, which signals potential mandatrophy. However, the constraint retains a live coordination function (governance delivery to millions), so it is not a pure piton. The classification as tangled_rope captures the hybrid: coordination has not atrophied entirely, but extraction dominates the residual mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_textual_sovereignty_ambiguity,
    'Does the English Article I text support the claim of complete sovereignty cession, or does the MÄori text''s use of kÄwanatanga versus tino rangatiratanga create irreducible ambiguity about what authority was transferred?',
    'Historical-linguistic analysis of the 1840 signings, comparative jurisprudence on bilingual treaties, and assessment of whether chiefs understood they were ceding absolute sovereignty.',
    'Resolution favoring MÄori text authority would reclassify the Crown sovereignty reading toward snare (pure extraction through legal fiction); resolution favoring English text exclusivity would reinforce the tangled_rope classification (genuine coordination with asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_textual_sovereignty_ambiguity, conceptual, 'Textual ambiguity between English and MÄori Treaty versions over sovereignty cession').

omega_variable(
    mandatrophy_of_sovereignty_doctrine,
    'Has the doctrine of parliamentary supremacy outlived its founding purpose of establishing settler governance, and does it now persist primarily by inertia despite modern recognition of Treaty partnership obligations?',
    'Constitutional reform analysis assessing whether a transition to bi-cultural or Treaty-based constitutional arrangements is structurally blocked by the supremacy doctrine.',
    'If the founding problem is dead and the constraint persists with high theater, reclassification toward piton; if the coordination function remains live but extraction dominates, remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_sovereignty_doctrine, empirical, 'Whether parliamentary supremacy mandate has atrophied').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of rangatiratanga alternatives structural (legal bars, jurisdictional denial, resource deprivation) or internalized (acceptance of Crown authority as legitimate within some MÄori communities)?',
    'Comparative analysis of MÄori political mobilization and jurisdictional revival: if suppressed alternatives re-emerge rapidly when structural bars are lifted, suppression is primarily structural.',
    'If internalized, effective suppression exceeds structural measures and the constraint''s resilience is higher; if purely structural, resistance may be more effective than metrics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression of MÄori authority alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waitangi_crown_tr_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(waitangi_crown_tr_t20, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(waitangi_crown_tr_t40, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(waitangi_crown_tr_t80, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(waitangi_crown_tr_t120, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 120, 0.4).
narrative_ontology:measurement(waitangi_crown_tr_t160, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 160, 0.38).
narrative_ontology:measurement(waitangi_crown_tr_t184, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 184, 0.35).

% Extraction over time
narrative_ontology:measurement(waitangi_crown_be_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(waitangi_crown_be_t20, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(waitangi_crown_be_t40, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 40, 0.88).
narrative_ontology:measurement(waitangi_crown_be_t80, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 80, 0.92).
narrative_ontology:measurement(waitangi_crown_be_t120, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 120, 0.9).
narrative_ontology:measurement(waitangi_crown_be_t160, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 160, 0.85).
narrative_ontology:measurement(waitangi_crown_be_t184, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 184, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(waitangi_crown_su_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(waitangi_crown_su_t20, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(waitangi_crown_su_t40, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(waitangi_crown_su_t80, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 80, 0.85).
narrative_ontology:measurement(waitangi_crown_su_t120, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 120, 0.8).
narrative_ontology:measurement(waitangi_crown_su_t160, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 160, 0.78).
narrative_ontology:measurement(waitangi_crown_su_t184, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 184, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the waitangi_sovereignty_allocation kernel. The English Article I cession reading is structurally distinct from partnership and rangatiratanga readings, which instantiate different constraints with different epsilon values and stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
