% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Orthographic Legitimacy: Modernist Reading (Rupture from Ottoman Past)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'modernist_reading' of the orthographic
 *   legitimacy kernel, focusing on the Turkish script reform of 1928. From
 *   this perspective, orthographic change was a foundational act to align the
 *   new Turkish Republic with Western/European modernity and decisively
 *   rupture from its Ottoman/Islamic past. The reform rendered traditional
 *   elites functionally illiterate, extracting cultural and political capital
 *   from them while consolidating the power and identity of the modernizing
 *   state apparatus. The claimed type is 'tangled_rope' because it served a
 *   genuine coordination function (national identity formation,
 *   administrative standardization) for the state, but simultaneously imposed
 *   severe, asymmetric extraction on traditional groups through active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.85).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.9).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Orthographic Legitimacy: Modernist Reading (Rupture from Ottoman Past)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, '36926e42-b733-4bad-a468-9f3f41fb6f77').
narrative_ontology:cs_kernel_codification('36926e42-b733-4bad-a468-9f3f41fb6f77', formalized).
narrative_ontology:cs_authority_grounding('36926e42-b733-4bad-a468-9f3f41fb6f77', extraction).
narrative_ontology:cs_interpretation_layer_present('36926e42-b733-4bad-a468-9f3f41fb6f77').
narrative_ontology:cs_reading_relation('36926e42-b733-4bad-a468-9f3f41fb6f77', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('36926e42-b733-4bad-a468-9f3f41fb6f77', orthographic_legitimacy_kernel__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('36926e42-b733-4bad-a468-9f3f41fb6f77', foundational, national_identity_through_rupture).
narrative_ontology:cs_axiom_status(national_identity_through_rupture, holdable).
narrative_ontology:cs_axiom_grounding('36926e42-b733-4bad-a468-9f3f41fb6f77', national_identity_through_rupture, deontological).
narrative_ontology:cs_axiom('36926e42-b733-4bad-a468-9f3f41fb6f77', foundational, western_modernity_as_legitimacy).
narrative_ontology:cs_axiom_status(western_modernity_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('36926e42-b733-4bad-a468-9f3f41fb6f77', western_modernity_as_legitimacy, conventional).
narrative_ontology:cs_reference_frame('36926e42-b733-4bad-a468-9f3f41fb6f77', western_aligned_national_identity).
narrative_ontology:cs_drift_state('36926e42-b733-4bad-a468-9f3f41fb6f77', contemporary_globalized_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('36926e42-b733-4bad-a468-9f3f41fb6f77', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, secular_elites).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, religious_scholars).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, traditional_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state institutions that decreed and enforced the orthographic reform, aiming to forge a new national identity and streamline administration. They benefit from the symbolic rupture with the past and the consolidation of a new, secular national narrative.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Intellectuals, bureaucrats, and political figures who championed the modernist project. They gained social and political capital by aligning with the new orthography, which became a marker of their progressive identity and access to power.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, secular_elites, beneficiary,
    powerful, biographical, mobile, national).

% Individuals educated in the Ottoman script, including writers, administrators, and merchants. They were rendered functionally illiterate overnight, losing their professional standing, access to public life, and cultural fluency in the new system.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class, payer,
    powerless, biographical, trapped, national).

% Custodians of religious texts and traditional Islamic education, whose knowledge was primarily encoded in the Ottoman script. The reform severed their connection to new generations, eroding their authority and the continuity of religious scholarship.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, religious_scholars, payer,
    powerless, generational, identity_locked, national).

% Rural and conservative populations whose cultural memory, historical records, and local traditions were often tied to the old script. They experienced a profound cultural discontinuity and alienation from the new state-sponsored identity.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, traditional_communities, payer,
    powerless, generational, identity_locked, local).

% Individuals and groups who argued for the preservation of the Ottoman script due to its historical, religious, and literary significance. Their voices were marginalized and suppressed by the dominant modernist narrative and state power.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, continuity_advocates, excluded,
    moderate, biographical, constrained, national).

% Reformers primarily concerned with maximizing literacy rates and administrative efficiency, who supported script change but might have prioritized different outcomes or justifications than the modernist rupture. They observe the long-term societal impacts.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, instrumentalist_reformers, observer,
    powerful, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__modernist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a new, distinct national identity aligned with Western modernity, facilitating administrative and educational reforms by standardizing a new orthography and symbolically rupturing from the Ottoman past.
% TRANSFER_FUNCTION: Transfers cultural, political, and social capital from traditional Ottoman/Islamic institutions and elites to the new secular, modernizing state apparatus and its aligned elites, by rendering the former's linguistic and cultural foundation obsolete.
% ABSENT_VOICES: The Ottoman literate class, religious scholars, and traditional communities were largely excluded from the decision-making process; their objections to the cultural rupture and loss of heritage were suppressed by the state's authority and the dominant modernist narrative.
% DISAPPEARANCE_RATIONALE: If the orthographic reform and its underlying modernist legitimacy vanished, the national identity, educational system, administrative structures, and historical narratives would fundamentally destabilize. The very foundation of the modern state, built on this rupture, would require a complete re-evaluation.
% FOUNDING_PROBLEM: The perceived backwardness and illiteracy associated with the Ottoman script, and the desire to forge a new national identity distinct from the Ottoman past, aligning with a modern, secular, Western-oriented vision.
% FOUNDING_PROBLEM_CORROBORATION: The modernizing state apparatus and secular intellectuals attest that the problem of maintaining a distinct, modern national identity is still live. Historians and cultural critics, from outside the direct beneficiaries, corroborate the historical context of the perceived need for modernization and rupture, even if they critique the methods or consequences of the reform.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) due to the profound and sudden loss of literacy, status, and cultural access for large segments of the population. Suppression is very high (0.90) as the reform was a top-down, state-enforced mandate with severe penalties for non-compliance and no viable alternatives. Theater ratio is low (0.10) because the reform was a genuine, functional transformation of national identity and state administration, not merely performative. Accessibility collapse is high (0.90) as the old script was effectively banned from official use, making it inaccessible for public life. Resistance is high (0.70) given the significant cultural and social upheaval it caused, though this resistance was largely suppressed. The temporal measurements reflect an initial period of intense enforcement and extraction, which then stabilized as the new orthography became entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the modernizing state apparatus and secular elites, the orthographic reform was a necessary and beneficial act of national renewal and progress. From the perspective of the Ottoman literate class, religious scholars, and traditional communities, it was a catastrophic act of cultural destruction and forced illiteracy. The engine's classification will highlight this divergence by computing different effective extraction values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernizing state apparatus and secular elites are clear beneficiaries, as the reform directly served their goals of national identity formation and power consolidation (low directionality). The Ottoman literate class, religious scholars, and traditional communities are the primary targets, bearing the direct costs of illiteracy, loss of status, and cultural discontinuity (high directionality). Instrumental reformers are observers, while continuity advocates are excluded, their positions actively suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumental_vs_constitutive_function,
    'Was the orthographic change primarily instrumental (e.g., for literacy, efficiency) or constitutive (e.g., for national identity, symbolic rupture)?',
    'Comparative analysis with other script reforms that prioritized literacy without explicit rupture, examining long-term societal outcomes and stated political goals.',
    'If primarily instrumental, the extraction might be re-evaluated as a high but temporary cost of coordination; if primarily constitutive, the extraction is integral to the identity transformation and less amenable to ''efficiency'' critiques.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_vs_constitutive_function, conceptual, 'Distinguishing the primary driver and nature of the orthographic reform.').

omega_variable(
    objectivity_of_script_backwardness,
    'To what extent was the ''backwardness'' of the Ottoman script an objective linguistic or pedagogical fact versus a political construct used to justify the rupture?',
    'Linguistic and historical analysis of the Ottoman script''s adaptability to modern Turkish phonology and its pedagogical efficacy, independent of political narratives.',
    'If largely a political construct, the justification for the reform''s severity is weakened, amplifying the perceived extraction; if objectively backward, some extraction might be seen as an unavoidable cost of necessary modernization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectivity_of_script_backwardness, empirical, 'Assessing the factual basis of the reform''s primary justification.').

omega_variable(
    long_term_cultural_memory_impact,
    'What is the long-term impact of the orthographic rupture on cultural memory, historical access, and intergenerational transmission of knowledge within the affected communities?',
    'Longitudinal sociological and historical studies examining literacy rates in the old script, access to historical documents, and the vitality of traditional cultural practices across generations.',
    'If the long-term impact is severe and persistent cultural amnesia, the extraction''s magnitude is confirmed or even amplified; if communities found ways to bridge the gap, the severity might be re-evaluated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_cultural_memory_impact, empirical, 'Evaluating the enduring consequences of the script change on cultural continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 1928, 1978).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1928, 0.05).
narrative_ontology:measurement(orth_tr_t1938, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1938, 0.08).
narrative_ontology:measurement(orth_tr_t1948, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(orth_tr_t1958, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(orth_tr_t1968, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1968, 0.11).
narrative_ontology:measurement(orth_tr_t1978, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1978, 0.1).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1928, 0.75).
narrative_ontology:measurement(orth_be_t1938, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1938, 0.8).
narrative_ontology:measurement(orth_be_t1948, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1948, 0.83).
narrative_ontology:measurement(orth_be_t1958, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1958, 0.85).
narrative_ontology:measurement(orth_be_t1968, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1968, 0.86).
narrative_ontology:measurement(orth_be_t1978, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1978, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1928, 0.85).
narrative_ontology:measurement(orth_su_t1938, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1938, 0.9).
narrative_ontology:measurement(orth_su_t1948, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1948, 0.92).
narrative_ontology:measurement(orth_su_t1958, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1958, 0.9).
narrative_ontology:measurement(orth_su_t1968, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1968, 0.88).
narrative_ontology:measurement(orth_su_t1978, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1978, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, national_education_system_reform).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, secular_legal_framework).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, turkish_national_identity_construction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
