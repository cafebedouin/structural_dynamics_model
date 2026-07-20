% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Originalist Reading: Equality Bounded by Founding-Era Social Taxonomy
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   The originalist reading of 'all men are created equal' treats the
 *   Declaration's equality principle and the Constitution's equal protection
 *   as semantically bound by the social taxonomy of the founding era. Under
 *   this reading, 'all men' did not include enslaved Africans, women, or
 *   indigenous peoples, and the scope of constitutional equality is
 *   permanently indexed to that historical understanding. The constraint
 *   extracts from historically excluded groups by denying them the protective
 *   scope of equality principles while benefiting the descendants of the
 *   founding elite through the preservation of their privileged legal and
 *   political position. The reading is presented as objective historical
 *   interpretation but functions as a structural barrier to equality
 *   expansion. This JSON instantiates ONLY the originalist reading; sibling
 *   readings (universalist, textualist_paradox) are separate constraints in
 *   the same kernel family.
 *
 * KEY AGENTS:
 *   - originalist_judiciary_and_scholars: Agenda-setter (institutional/arbitrage) â administers the interpretive method.
 *   - founding_elite_descendants: Primary beneficiary (powerful/mobile) â retain privileged access to political equality.
 *   - enslaved_african_americans: Primary target (powerless/trapped) â bear extraction through legal non-personhood and subsequent regimes.
 *   - women: Secondary target (powerless/constrained) â excluded from political and legal equality by founding-era taxonomy.
 *   - indigenous_peoples: Secondary target (powerless/constrained) â excluded from the polity and its equality guarantees.
 *   - universalist_reformers: Excluded voice (moderate/constrained) â advocate expansive equality but are marginalized.
 *   - constitutional_historians: Analytical observer (analytical/analytical) â test originalist empirical claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.82).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.75).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Reading: Equality Bounded by Founding-Era Social Taxonomy").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '614f00ab-121a-41fd-853a-454e0b96dab6').
narrative_ontology:cs_kernel_codification('614f00ab-121a-41fd-853a-454e0b96dab6', fixed_text).
narrative_ontology:cs_authority_grounding('614f00ab-121a-41fd-853a-454e0b96dab6', lineage).
narrative_ontology:cs_interpretation_layer_present('614f00ab-121a-41fd-853a-454e0b96dab6').
narrative_ontology:cs_reading_relation('614f00ab-121a-41fd-853a-454e0b96dab6', all_men_created_equal__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('614f00ab-121a-41fd-853a-454e0b96dab6', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('614f00ab-121a-41fd-853a-454e0b96dab6', foundational, original_public_meaning_governs_equality).
narrative_ontology:cs_axiom_status(original_public_meaning_governs_equality, holdable).
narrative_ontology:cs_axiom_grounding('614f00ab-121a-41fd-853a-454e0b96dab6', original_public_meaning_governs_equality, conventional).
narrative_ontology:cs_reference_frame('614f00ab-121a-41fd-853a-454e0b96dab6', founding_era_social_taxonomy).
narrative_ontology:cs_drift_state('614f00ab-121a-41fd-853a-454e0b96dab6', contemporary_human_rights_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('614f00ab-121a-41fd-853a-454e0b96dab6', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_african_americans).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_peoples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the originalist interpretive method in constitutional adjudication and scholarship. They deploy historical evidence to fix the semantic scope of equality clauses to 18th-century public meaning, enforcing the boundary through judicial opinions, law school curricula, and clerkship networks. Their professional authority and research programs depend on the method, but they do not personally capture the material extraction.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_judiciary_and_scholars, agenda_setter,
    institutional, generational, arbitrage, national).

% Inherit the structural advantages of the 18th-century social taxonomy that the originalist reading preserves: preferential access to political participation, property rights, and civic equality through the exclusion of others. They benefit from the stability of hierarchy without needing to actively maintain the interpretive apparatus.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_descendants, beneficiary,
    powerful, generational, mobile, national).

% Bear the maximal extraction under this reading: legal non-personhood, chattel slavery, and subsequent Jim Crow segregation justified by originalist arguments that the founders did not intend Black civic equality. Their descendants continue to face voter suppression and carceral systems legitimated by originalist constitutional boundaries.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_african_americans, payer,
    powerless, biographical, trapped, national).

% Excluded from the category of 'all men' in the founding-era taxonomy and therefore from suffrage, property rights, and equal protection until mass mobilization forced partial inclusion. Originalist readings continue to resist gender-equality claims by appealing to 18th-century domestic relations law.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women, payer,
    powerless, generational, constrained, national).

% Excluded from the polity and from the equality guarantee by founding-era taxonomy that treated them as foreign nations or domestic dependents. Their land sovereignty and political rights are continuously constrained by originalist readings of the Constitution's Indian Commerce Clause and plenary power doctrine.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_peoples, payer,
    powerless, generational, constrained, national).

% Advocate for reading constitutional equality as an inclusive, aspirational principle that expands beyond the founders' specific intentions. They are structurally marginalized in originalist jurisprudence, where their arguments are dismissed as illegitimate moral philosophy rather than law.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_reformers, excluded,
    moderate, biographical, constrained, national).

% Provide empirical historical research on 18th-century language, social taxonomy, and political thought. Their findings are selectively appropriated by originalists and universalists alike; from their seat, the evidentiary record is often underdetermined enough to support multiple normative conclusions.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes the semantic content of constitutional text to an objectively discoverable historical meaning, providing a purportedly neutral method for resolving interpretive disagreement and constraining judicial discretion across generations.
% TRANSFER_FUNCTION: Moves the authority to define the boundaries of legal and political equality from historically excluded groups and future generations to the 18th-century propertied elite and their interpretive successors; the cost of equality claims is borne by those outside the founders' social taxonomy, while the benefit of stable hierarchical order accrues to the dominant group.
% ABSENT_VOICES: Enslaved persons, women, indigenous peoples, and their descendants are treated as historically excluded rather than wrongfully excluded; universalist moral philosophers and critical race theorists who would read the text as mandating inclusion are structurally absent from originalist interpretive frameworks.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished overnight, constitutional equality doctrine would reorganize around universalist, living-constitutionalist, or moral-readings principles. Statutes and state actions previously shielded by original-history arguments would become vulnerable to equality challenges, and the judiciary would lose its primary technique for resisting expansive equality claims.
% FOUNDING_PROBLEM: How to maintain a written constitution as binding law across generations without its meaning becoming a mere reflection of transient contemporary majorities or unaccountable judicial preferences.
% FOUNDING_PROBLEM_CORROBORATION: The Federalist Society and originalist jurists attest the problem is still live, citing judicial activism. Critical race theorists, feminist legal scholars, and historians of slavery attest the problem has shifted to overcoming founding-era injustice; no corroborating source outside the benefiting interpretive community confirms that originalism is the necessary solution to judicial legitimacy rather than a mechanism for preserving hierarchy.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint systematically transfers the capacity for equality-claims from excluded groups to the dominant order. Suppression is high (0.75) because alternative universalist interpretations are actively excluded from originalist jurisprudence and institutional power. Theater ratio is substantial (0.60) because the constraint is maintained through an elaborate scholarly and judicial apparatus that performs historical objectivity; the research is often genuine, but its deployment to bound equality is performative in that it treats contingent historical exclusion as constitutional necessity. Accessibility collapse is high (0.80) because once one accepts the originalist frame, alternative equality readings appear as illegitimate judicial activism. Resistance is moderate (0.45) because excluded groups and allies have consistently resisted, though often through constitutional amendment rather than interpretive victory. The temporal measurements show a cyclical pattern: extraction peaks during antebellum and Jim Crow eras, dips during Reconstruction and the Civil Rights revolution when universalist pressure temporarily overrides originalist boundaries, then rebounds as modern originalism re-encodes exclusion in neutral methodological language.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judiciary experiences the constraint as genuine coordination (solving judicial legitimacy by fixing constitutional meaning), while excluded groups experience it as enforced extraction that preserves their subordination. The engine will compute divergent seat types: the agenda-setter seat likely computes as rope or tangled_rope, while the victim seats compute as snare. The perspectival gap is the central measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   The founding elite descendants are structural beneficiaries (low d, subsidized by the constraint's preservation of their privileged legal status). The originalist judiciary sits near symmetric or mild beneficiary (their professional authority depends on the method, but they do not personally capture the material extraction). Excluded groupsâenslaved African Americans, women, indigenous peoplesâare full targets (high d, high effective extraction). Universalist reformers are excluded from the interpretive framework and bear the epistemic cost of marginalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading prevents mislabeling by distinguishing the genuine coordination function of historical interpretation (stabilizing constitutional meaning across generations) from the extraction function (using that stabilization to lock in 18th-century exclusions). The metrics independently measure high extraction and substantial theater, preventing the coordination story from masking the extraction. The founding problemâjudicial legitimacyâ is contested as still live, signaling potential mandatrophy: the originalist solution may have outlived the problem of judicial activism and become a barrier to addressing substantive equality. The R5 mismatch (founding_problem_status: contested Ã disappearance_verdict: world_rearranges) flags this constraint as a potential zombie mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the originalist reading describe a binding historical fact about constitutional meaning, or does it function as a structural mechanism to preserve 18th-century hierarchies under the guise of objective interpretation?',
    'Sociological and doctrinal analysis comparing originalist jurisprudence outcomes against stated methodological commitments; longitudinal study of which equality claims succeed or fail under originalist frameworks.',
    'If the latter, the constraint is more extractive than its coordination story suggests, pushing victim-seat classifications toward snare; if the former, the extraction is epiphenomenal to genuine historical recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether originalism is discovery or construction of exclusion.').

omega_variable(
    founders_intent_empirical_basis,
    'Is the founders'' intent regarding ''all men are created equal'' historically determinate, or does the evidentiary record underdetermine original public meaning in ways that allow projection of modern exclusions backward?',
    'Corpus linguistics, archival reception history, and transatlantic print-culture analysis of ''all men'' and ''equal'' in the 1760-1790 anglophone world.',
    'If underdetermined, the originalist reading''s extraction is partially fabricated rather than discovered, raising base_extractiveness and undermining the coordination claim to objectivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founders_intent_empirical_basis, empirical, 'Empirical determinacy of original public meaning.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the originalist reading logically foreclose the universalist reading within a single interpretive framework, or do they coexist as alternative commitment systems held by different parties?',
    'Logical analysis of whether original-intent bindingness is compatible with iterative universal expansion regardless of intent; survey of jurists who hold intermediate or hybrid positions.',
    'If foreclosure is genuine, the kernel produces a hierarchy of displacement; if mere competition, the kernel produces a constraint family of coexisting readings with different directionalities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between originalist and universalist readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative equality readings structural (enforced by courts and sanctions) or internalized (targets believe the originalist taxonomy is natural or legitimate)?',
    'Post-exit trajectory analysis: if excluded groups continue to accept originalist boundaries after gaining formal political power, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure, amplifying victim-seat extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__originalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(all__tr_t48, all_men_created_equal__originalist_reading, theater_ratio, 48, 0.2).
narrative_ontology:measurement(all__tr_t96, all_men_created_equal__originalist_reading, theater_ratio, 96, 0.35).
narrative_ontology:measurement(all__tr_t144, all_men_created_equal__originalist_reading, theater_ratio, 144, 0.5).
narrative_ontology:measurement(all__tr_t192, all_men_created_equal__originalist_reading, theater_ratio, 192, 0.55).
narrative_ontology:measurement(all__tr_t240, all_men_created_equal__originalist_reading, theater_ratio, 240, 0.6).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__originalist_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(all__be_t48, all_men_created_equal__originalist_reading, base_extractiveness, 48, 0.92).
narrative_ontology:measurement(all__be_t96, all_men_created_equal__originalist_reading, base_extractiveness, 96, 0.75).
narrative_ontology:measurement(all__be_t144, all_men_created_equal__originalist_reading, base_extractiveness, 144, 0.88).
narrative_ontology:measurement(all__be_t192, all_men_created_equal__originalist_reading, base_extractiveness, 192, 0.78).
narrative_ontology:measurement(all__be_t240, all_men_created_equal__originalist_reading, base_extractiveness, 240, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__originalist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(all__su_t48, all_men_created_equal__originalist_reading, suppression_requirement, 48, 0.9).
narrative_ontology:measurement(all__su_t96, all_men_created_equal__originalist_reading, suppression_requirement, 96, 0.65).
narrative_ontology:measurement(all__su_t144, all_men_created_equal__originalist_reading, suppression_requirement, 144, 0.85).
narrative_ontology:measurement(all__su_t192, all_men_created_equal__originalist_reading, suppression_requirement, 192, 0.8).
narrative_ontology:measurement(all__su_t240, all_men_created_equal__originalist_reading, suppression_requirement, 240, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'all_men_created_equal.' The originalist reading and its siblings (universalist, textualist_paradox) are structurally distinct constraints with different Îµ values, beneficiary/victim structures, and classifications. They form a constraint family linked by mutual network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
