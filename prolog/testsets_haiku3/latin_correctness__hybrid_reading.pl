% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Bifurcated Latin Correctness: Classical Literary vs. Medieval Technical
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The hybrid reading of Latin correctness emerges in the late medieval and
 *   early Renaissance period as a compromise between classical purist claims
 *   and practical medieval realities. It asserts that classical norms apply
 *   legitimately to literary, rhetorical, and philosophical domains, while
 *   medieval forms remain legitimate in technical, administrative, and
 *   theological domains. This reading is neither the continuity view
 *   (medieval Latin is organic evolution deserving equal prestige) nor the
 *   rupture view (classical Latin is a fixed standard and medieval usage is
 *   corruption). Instead, it bifurcates legitimacy: creates a hierarchy where
 *   classical work in literary domains ranks higher than medieval work in
 *   technical domains, even where medieval forms are functionally superior
 *   for their purposes. The constraint operates through the establishment's
 *   control of prestige, publication access, and institutional judgment,
 *   enforcing classical standards in prestigious domains while grudgingly
 *   acknowledging medieval legitimacy in subordinate domains.
 *
 * KEY AGENTS:
 *   - Classical literary establishment: institutional beneficiary and primary agenda-setter; sets the standard and controls prestige
 *   - Humanist rhetoricians: powerful beneficiary in the classical domain; has rhetorical resources to defend classical norms
 *   - Medieval technical writers: moderate power but identity-locked payers; pressured toward unattainable classical standards in formal contexts
 *   - Monastic scholars: organized payer-beneficiary; their domain is acknowledged as legitimate but subordinate in status
 *   - Scribal copyists: powerless trapped agents; bear direct labor cost of navigating contradictory legitimacy signals
 *   - Ecclesiastical authority: institutional beneficiary-setter; legitimizes medieval forms but within a classical-prestige framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.58).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.62).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Bifurcated Latin Correctness: Classical Literary vs. Medieval Technical").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, 'f3f26ed5-cc28-4680-ab6c-aad5d250eb30').
narrative_ontology:cs_kernel_codification('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', fixed_text).
narrative_ontology:cs_authority_grounding('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', lineage).
narrative_ontology:cs_interpretation_layer_present('f3f26ed5-cc28-4680-ab6c-aad5d250eb30').
narrative_ontology:cs_reading_relation('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', foundational, domain_bifurcation_legitimate).
narrative_ontology:cs_axiom_status(domain_bifurcation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', domain_bifurcation_legitimate, conventional).
narrative_ontology:cs_axiom('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', foundational, classical_prestige_hierarchy).
narrative_ontology:cs_axiom_status(classical_prestige_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', classical_prestige_hierarchy, conventional).
narrative_ontology:cs_reference_frame('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', latin_as_unified_standard).
narrative_ontology:cs_drift_state('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', renaissance_humanist_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f3f26ed5-cc28-4680-ab6c-aad5d250eb30', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_literary_establishment).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, humanist_rhetoricians).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, medieval_technical_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, monastic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, monastic_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, ecclesiastical_authority).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, scribal_copyists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and adjudicates the standard of 'correct Latin' in literary, rhetorical, and philosophical domains. Maintains that classical forms (Cicero, Virgil, the approved authors) embody the correct use and that medieval innovations are corruptions. Controls access to prestige, publication, and academic standing through this standard. Benefits from the bifurcation because it reserves the highest status for classical-aligned work in their domain.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_literary_establishment, agenda_setter,
    institutional, generational, arbitrage, continental).

% Operate in literary and rhetorical domains where classical norms are legitimized by the establishment. Have rhetorical and social resources to argue for classical standards and can shift between domains. Benefit from the prestige and clarity that comes with a single, canonical standard in their work. Face no enforcement pressure because their domain aligns with the establishment's rules.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, humanist_rhetoricians, beneficiary,
    powerful, biographical, mobile, continental).

% Work in technical, practical, and administrative domains where medieval Latin forms are genuinely more functional (specialized vocabulary, inflectional patterns suited to legal and scientific precision). The hybrid reading acknowledges their forms are legitimate in their domain, but they remain stigmatized as 'barbarous' by the literary establishment. Pressured to adopt classical forms in formal writing even when medieval forms are more precise; their technical vocabulary and neologisms are marked as errors rather than solutions. Career advancement requires navigation of contradictory legitimacy: their work is functional in medieval forms but marked as incorrect by external judges.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, medieval_technical_writers, payer,
    moderate, biographical, identity_locked, regional).

% Produce theological, liturgical, and practical texts in medieval Latin forms developed through centuries of monastic writing tradition. The hybrid reading legitimizes their forms within their domain, but they carry lower institutional status than classical rhetoricians. Benefit from domain-specific legitimacy; bear the cost of being classified as subordinate to the literary standard. Their textual tradition is continuous and functional but treated as a degraded version of classical usage by external critics.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, monastic_scholars, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, monastic_scholars, beneficiary).

% Reproduce texts in whatever Latin forms the commission requires. When copying classical texts, they follow established exemplars. When copying medieval texts, they receive contradictory signals: preserve the original medieval forms (their primary duty) or 'correct' them toward classical standards (the external judgment). The bifurcation creates impossible labor: conform to source or conform to judgment. The lowest-power agent in the constraint, bearing enforcement pressure from multiple directions.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, scribal_copyists, payer,
    powerless, immediate, trapped, local).

% Legitimizes medieval Latin for liturgical and theological purposes (Latin must accommodate Christian neologisms and liturgical needs that classical Latin does not serve). Controls monastic writing through commission and approval. Benefits from the bifurcation because it reserves the highest prestige for classical-style writing while protecting medieval forms' functional legitimacy in their sphere. Partially enforces the standard by commissioning classical-style works and marking medieval work as subordinate.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, ecclesiastical_authority, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, ecclesiastical_authority, agenda_setter).

% The corpus of approved classical texts (Cicero, Virgil, Horace, Augustine) functions as the fixed reference point. Not an agent but a non-agent entity maintained through the constraint: the texts themselves are elevated to canonical status and treated as the measure of correctness. Collects no rents but is the authority structure's legitimizing object.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_authors_estate, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(latin_correctness__hybrid_reading, classical_authors_estate).

% Evaluate and adjudicate Latin quality in emerging universities. Positioned as neutral arbiters but inherit the classical standard and teach it as normative. Their judgments reinforce the status hierarchy: classical-aligned work receives higher marks, medieval forms are corrected. Can influence the constraint through curriculum design and grading, but are also constrained by institutional prestige dynamics that favor classical authority.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, university_masters, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, classical_literary_establishment).
narrative_ontology:fixing_cost_class(latin_correctness__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for evaluating Latin correctness across geographically dispersed writing communities, enabling comparative judgment and textual exchange. Within each domain (literary vs. technical), creates predictability about what forms are acceptable.
% TRANSFER_FUNCTION: Transfers prestige, institutional standing, and access to authoritative publication from medieval technical writers and monastic scholars to classical literary establishment and humanist rhetoricians. Transfers interpretive authority from living medieval writing communities to the corpus of classical approved authors. Transfers labor from copyists and technical writers who must navigate contradictory legitimacy standards.
% ABSENT_VOICES: Practitioners of practical Latin forms in commerce, law, and administration who are not monastic scholars and have no institutional platform to defend their usage. Scribal workers in scriptoria who follow medieval exemplars but are corrected by external judges. Living technical writers in post-classical domains (medieval medicine, mathematics, law) whose innovations are marked as barbarism rather than functional adaptation.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, technical and practical Latin writing would no longer carry stigma; monastic textual traditions would be studied as evolving practices rather than corruptions; copyists would reproduce without pressure to 'correct' medieval forms; the prestige differential between literary and technical domains would flatten. The corpus of classical texts would remain important as sources but would not function as the singular measure of correctness.
% FOUNDING_PROBLEM: Early medieval period faced a fragmented Latin landscape: classical literary standards were maintained in scholarly contexts, but Latin as a living language had evolved new forms to serve Christian theology, administrative needs, and technical precision. The hybrid reading emerged as a way to preserve classical authority in prestigious domains while acknowledging that medieval forms were functional and necessary in practical domains.
% FOUNDING_PROBLEM_CORROBORATION: Medieval monks and technical writers attest that classical forms are impractical for their domains and that medieval innovations solved real problems (neologisms for Christian concepts, inflectional precision for legal documents, specialized vocabulary for medicine). Humanist scholars attested in Renaissance writings that classical standards must be preserved as a cultural anchor. Modern historical linguists (from outside the benefiting literary establishment) attest that medieval Latin was a coherent, functional register adapted to its purposes, not a corruption of classical usage.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.58 captures moderate but real transfer: the classical literary establishment gains prestige, institutional standing, and interpretive authority; medieval technical writers lose prestige and career-advancement signals, even where their forms are functionally superior. Suppression at 0.62 reflects active enforcement: the establishment must continuously police the boundary, mark medieval forms as errors, and pressure copyists to 'correct' texts. The enforcement is lighter than a pure snare because the hybrid reading does acknowledge medieval forms as legitimate in their domains — but that acknowledgment comes with a prestige penalty. Theater_ratio at 0.41 reflects the performative work of maintaining the hierarchy: constant invocation of classical exemplars in literary judgment, marking medieval forms as 'barbarous' or 'corrupted,' even when the substance of correctness is functional adequacy within a domain. The measurement series shows extractiveness rising from 0.42 to 0.58 over the interval (t0 to t40), indicating that the prestige differential and enforcement intensity increase as Renaissance humanism hardens the classical standard and institutional positions consolidate. Theater_ratio also rises, showing that maintaining the bifurcation requires increasing performative work — the establishment must more actively defend classical supremacy as medieval forms prove their practical utility.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (medieval technical writers, monastic scholars) experience the constraint as an asymmetric legitimacy hierarchy enforced through prestige and institutional judgment. The agenda-setter and beneficiary seats experience it as genuine coordination around a functional standard. Copyists, as the lowest-power agent, experience pure suppression without clear benefit. The divergence arises from exit options (arbitrage vs. identity-lock vs. trapped) and power differentials: high-power beneficiaries have the resources to defend the standard; low-power payers cannot challenge it.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical literary establishment: d ≈ 0.15 (near beneficiary end) — collects prestige, controls judgment, arbitrage-grade exit options (can shift domains or critique the standard). Humanist rhetoricians: d ≈ 0.20 (beneficiary) — benefits from prestige alignment, mobile within their domain. Medieval technical writers: d ≈ 0.75 (near target end) — pay prestige cost, bear career risk, identity-locked (their technical expertise is inseparable from their identity in their domain), constrained exits (can only leave by abandoning their specialization). Monastic scholars: d ≈ 0.65 (target-leaning) — acknowledge legitimate domain but under prestige hierarchy, constrained by institutional commission and approval. Copyists: d ≈ 0.85 (near full target) — trapped, powerless, bear labor of navigating contradictory signals. Ecclesiastical authority: d ≈ 0.30 (moderate-beneficiary) — collects institutional standing from both classical prestige and medieval legitimacy; constrained but not trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinate around diverse Latin usage in post-classical context) is legitimately alive — technical domains still need functional Latin forms, literary domains still value classical authority. The hybrid reading resolves the mandatrophy by explicitly bifurcating: medieval forms are legitimate in technical domains (founding problem solved there), classical forms are legitimate in literary domains (founding problem solved there). However, the constraint extracts prestige cost by creating a hierarchy: this is not an error in the classification, but rather the true structure — the constraint is tangled rope because it genuinely coordinates around functional domains while simultaneously extracting prestige differentials. The mandatrophy is resolved, not because the problem disappeared, but because the bifurcation allows both solutions to coexist while embedding an asymmetry: technical domains work, but are treated as subordinate to literary domains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bifurcation_naturalness,
    'Is the domain bifurcation (literary vs. technical) a natural reflection of functional necessity, or an imposed status hierarchy that happens to align with functional domains?',
    'Examine whether the prestige differential persists when medieval forms demonstrably outperform classical forms in technical contexts. If prestige follows function (literary excellence when classical forms solve literary problems, technical excellence when medieval forms solve technical problems), the bifurcation is natural. If prestige hierarchy persists despite functional superiority of medieval forms in technical domains, the bifurcation is a status construct.',
    'If imposed (not natural), the constraint is more extractive than a pure coordination mechanism; it would shift from tangled rope toward snare. If natural, the constraint genuinely coordinates around functional domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bifurcation_naturalness, empirical, 'Whether the domain bifurcation reflects functional necessity or imposed status hierarchy.').

omega_variable(
    reading_foreclosure,
    'Does the hybrid reading logically rule out either the continuity reading or the rupture reading within a single coherent framework?',
    'Test whether a party holding the hybrid reading''s premises (domain bifurcation, legitimacy in both spheres, prestige hierarchy) could also coherently hold either sibling reading''s core premise (medieval as organic continuation, or medieval as corruption). The test is logical coherence within one framework, not historical possibility of holding multiple positions.',
    'If the hybrid reading forecloses one or both siblings within a single framework, the reading_relations should include ''forecloses'' edges. If both siblings remain logically coherent within the framework, they coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure, conceptual, 'Whether the hybrid reading''s core premise forecloses alternative readings within a unified framework.').

omega_variable(
    identity_lock_mechanism,
    'For medieval technical writers, is the identity-lock structural (their expertise IS medieval Latin forms, inseparable from their professional identity) or internalized (they have absorbed the classical standard''s prestige claim and believe medieval forms are genuinely inferior)?',
    'Post-constraint scenarios where the classical standard is removed: do technical writers quickly reclaim medieval forms as valuable and functional, or do they remain oriented toward classical forms even after enforcement ends? Persistence of classical preference after removal indicates internalization; immediate functional reversion indicates structural identity-lock.',
    'If internalized, the technical writers carry the suppression beyond the constraint''s removal; the effective suppression is higher than the structural measure. If structural, the suppression ends when enforcement ends.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether medieval technical writers'' identity-lock is structural or internalized in the classical standard.').

omega_variable(
    committer_relations,
    'How does this hybrid reading relate logically to the continuity and rupture readings of the same kernel?',
    'The hybrid reading asserts domain bifurcation; the continuity reading asserts organic evolution across all domains; the rupture reading asserts fixed classical standard across all domains. These are three structurally distinct positions on the same kernel commitment (Latin as shared standard). The committer structure is routed through omega variables (this field) rather than embedded in the constraint classification itself.',
    'Determines whether sibling readings are foreclosed or coexist with this reading. The reading_relations in cs_structure will declare these edges (forecloses, coexists_with, or influences).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_relations, conceptual, 'Structural relationship between this reading and its siblings in the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__hybrid_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(lati_tr_t0, observed).
narrative_ontology:measurement(lati_tr_t5, latin_correctness__hybrid_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(lati_tr_t5, observed).
narrative_ontology:measurement(lati_tr_t10, latin_correctness__hybrid_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(lati_tr_t10, observed).
narrative_ontology:measurement(lati_tr_t15, latin_correctness__hybrid_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(lati_tr_t15, observed).
narrative_ontology:measurement(lati_tr_t25, latin_correctness__hybrid_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(lati_tr_t25, observed).
narrative_ontology:measurement(lati_tr_t40, latin_correctness__hybrid_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(lati_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(lati_be_t0, observed).
narrative_ontology:measurement(lati_be_t5, latin_correctness__hybrid_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(lati_be_t5, observed).
narrative_ontology:measurement(lati_be_t10, latin_correctness__hybrid_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(lati_be_t10, observed).
narrative_ontology:measurement(lati_be_t15, latin_correctness__hybrid_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(lati_be_t15, observed).
narrative_ontology:measurement(lati_be_t25, latin_correctness__hybrid_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(lati_be_t25, observed).
narrative_ontology:measurement(lati_be_t40, latin_correctness__hybrid_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(lati_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(lati_su_t0, observed).
narrative_ontology:measurement(lati_su_t5, latin_correctness__hybrid_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(lati_su_t5, observed).
narrative_ontology:measurement(lati_su_t10, latin_correctness__hybrid_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(lati_su_t10, observed).
narrative_ontology:measurement(lati_su_t15, latin_correctness__hybrid_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement_basis(lati_su_t15, observed).
narrative_ontology:measurement(lati_su_t25, latin_correctness__hybrid_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(lati_su_t25, observed).
narrative_ontology:measurement(lati_su_t40, latin_correctness__hybrid_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(lati_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__hybrid_reading, 0.06).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel admits three structurally distinct constraint readings: continuity_reading (medieval Latin as legitimate evolution), rupture_reading (classical Latin as fixed standard), and this hybrid_reading (domain bifurcation with prestige hierarchy). Each reading instantiates a different ε, different beneficiary/victim structure, and different classification. They share a common kernel (Latin as shared written standard) but diverge on legitimacy and hierarchy. The three readings are linked via network.affects_constraints; do not attempt to fold them into one constraint with multiple observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__hybrid_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
