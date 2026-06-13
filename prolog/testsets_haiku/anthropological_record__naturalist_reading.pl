% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Anthropological Record—Naturalist Reading (Scientific Method Authority)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The naturalist reading of the anthropological record treats human origins
 *   as knowable exclusively through scientific method: genetic evidence of
 *   common descent, fossil chronology, paleolithic archaeology, and
 *   evolutionary theory. This is ONE reading of the contested kernel
 *   'anthropological record'; sibling readings instantiate creationist and
 *   indigenous epistemology frameworks. The naturalist reading extracts
 *   authority from non-credentialed interpreters and suppresses alternative
 *   epistemologies by defining them as non-empirical. The constraint is
 *   CLAIMED as tangled_rope (coordination via shared method + asymmetric
 *   extraction via credentialing), and the metrics reflect accumulating
 *   extractiveness over the interval as institutional gatekeeping intensified
 *   and competing frameworks were progressively marginalized. The claim and
 *   metrics are authored independently; the divergence between rising
 *   extractiveness and stable institutional coordination function is
 *   precisely the divergence the engine measures.
 *
 * KEY AGENTS:
 *   - Credentialed natural scientists: Set and enforce the epistemic standard; benefit from monopoly authority.
 *   - Academic anthropology institutions: Anchor institutional prestige to the naturalist reading; defend disciplinary boundary.
 *   - Indigenous knowledge holders: Hold oral traditions rooted in sustained community and place; suppressed by the identity-locking of 'non-empirical' designation.
 *   - Creationist communities: Hold scriptural readings; barred from credentialed forums by methodological naturalism.
 *   - Non-credentialed interpreters: Excluded from research access and publication venues; trapped by credentialing gates.
 *   - Public education systems: Required to teach naturalist framework; bear political cost of marginalizing other narratives.
 *   - Museum curators: Control physical artifacts and their interpretation; enforce naturalist narrative through curation.
 *   - Funding bodies: Route resources to naturalist research only; benefit from the constraint's focus and portability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.72).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Anthropological Record—Naturalist Reading (Scientific Method Authority)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, 'cece3f33-5f37-4f74-b49e-4ea90dbc9cdb').
narrative_ontology:cs_kernel_codification('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', fixed_text).
narrative_ontology:cs_authority_grounding('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', extraction).
narrative_ontology:cs_interpretation_layer_present('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb').
narrative_ontology:cs_reading_relation('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', foundational, methodological_naturalism_mandatory).
narrative_ontology:cs_axiom_status(methodological_naturalism_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', methodological_naturalism_mandatory, empirically_contingent).
narrative_ontology:cs_axiom('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', foundational, supernatural_causation_unfalsifiable).
narrative_ontology:cs_axiom_status(supernatural_causation_unfalsifiable, holdable).
narrative_ontology:cs_axiom_grounding('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', supernatural_causation_unfalsifiable, empirically_contingent).
narrative_ontology:cs_reference_frame('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', evolutionary_materialism_framework).
narrative_ontology:cs_drift_state('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', contemporary_identity_politics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cece3f33-5f37-4f74-b49e-4ea90dbc9cdb', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_natural_scientists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, academic_anthropology_institutions).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, creationist_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval because credentialing gates tightened: in the early period (t=0), alternative frameworks had some standing in public discourse and community interpretation; by t=40, credentialing has become effectively mandatory for institutional voice, internet accessibility has amplified both naturalist and creationist discourse while suppressing indigenous oral-tradition transmission, and funding concentration has reduced resource flow to non-naturalist research to near-zero. Suppression requirement rises similarly because maintaining the naturalist monopoly requires active defense against creationist political organizing, indigenous sovereignty claims over interpretation, and internet-enabled non-credentialed counter-narratives. Theater ratio (0.18 to 0.41) reflects the constraint's early coordination function (genuine methodological innovation solving real interpretive disputes) gradually eclipsed by its extractive function (defending institutional turf). The constraint is tangled_rope: the scientific method solves a real coordination problem (how to arbitrate disputed evidence across languages and nations without recourse to authority), AND it extracts authority from non-specialists and suppresses alternative epistemologies through institutional gatekeeping. Both functions are simultaneously present and active.
 *
 * PERSPECTIVAL GAP:
 *   From the naturalist institutional seat, the constraint is genuine coordination—a methodological innovation that allowed paleontology, genetics, and archaeology to speak to each other and resolve disputes through shared standards. From the indigenous and creationist seats, the constraint is a power grab dressed as epistemology—definitions of 'scientific' and 'empirical' were written to exclude non-materialist causation and oral-tradition epistemology from the start. The constraint satisfies both descriptions simultaneously: it IS a real coordination breakthrough AND it IS an extractive gatekeeping mechanism. The engine's per-seat computation captures this: the constraint computes as rope or tangled_rope from the scientist seat, snare from the victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed scientists sit at d ≈ 0.0 to 0.2 (high beneficiaries with arbitrage-grade exit—they could in principle leave academia and work as science communicators or explorers, but they have career and prestige locked in the system). Indigenous knowledge holders sit at d ≈ 0.85 to 0.95 (trapped by identity, excluded from institutional voice, suppressed when they try to interpret their own record). Creationist communities sit at d ≈ 0.65 to 0.75 (moderately mobile—they could adopt naturalism, but only by abandoning their theological framework, so exit is constrained by identity). Non-credentialed interpreters sit at d ≈ 0.80 (trapped by credentialing gates; their only exit is to become credentialed, which requires accepting the naturalist framework as prerequisite). Public education systems sit at d ≈ 0.55 to 0.65 (constrained exit—they could teach alternatives but face professional and funding pressure). The directionality spread is the seat divergence: from the naturalist seat, the constraint solves a problem; from the victim seats, it is the problem.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the false-rope classification by embedding genuine coordination (the scientific method does arbitrate evidence disputes across cultures) inside extractive gatekeeping (credentialing bars and suppression of alternative frameworks). A false-rope reading would focus only on the coordination benefit and miss the asymmetric extraction. A false-snare reading would focus only on the suppression and miss the genuine methodological innovation. The tangled_rope classification holds because both elements are structurally present and active: the constraint must be enforced to suppress alternatives (suppression = 0.72), and it coordinates research across languages and nations (coordination function is real), and it extracts authority asymmetrically (beneficiaries have monopoly on interpretation). None of the elements can be removed without collapsing the arrangement. This is precisely the tangled_rope structure: real coordination + real extraction + active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_naturalism_as_choice,
    'Is methodological naturalism (the restriction of scientific explanation to natural causation) a methodologically justified constraint derived from the logic of falsifiability, or is it a substantive philosophical choice that rules out non-natural explanations by definition rather than by evidence?',
    'Philosophical analysis of whether the exclusion of supernatural causation follows necessarily from empiricism or represents an additional metaphysical commitment. Examination of whether explanations that invoke non-natural causation could in principle be falsifiable and empirically constrained.',
    'If methodological naturalism is a methodological necessity, the naturalist reading excludes creationism by logic. If it is a philosophical choice, the exclusion is political or epistemic rather than logical, and the extraction becomes more visible as a deliberate gatekeeping mechanism rather than a methodological inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodological_naturalism_as_choice, conceptual, 'Whether the naturalist constraint''s authority comes from logical necessity or from a chosen philosophical stance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.72) primarily structural (legal barriers, funding gates, institutional access denial) or primarily internalized (individuals internalizing the judgment that their epistemology is ''non-empirical'' and self-censoring)?',
    'Post-institutional-softening trajectories: if suppression persists after formal barriers are removed (e.g., after indigenous land acknowledgments become standard and creationist legal challenges fail), the suppression is substantially internalized. If suppression drops when barriers are removed, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, because targets carry the suppression with them after exit. The constraint then operates partly through self-discipline rather than external force. This changes the remediation pathway: removing institutional barriers alone would be insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the credentialing constraint.').

omega_variable(
    indigenous_epistemology_accessibility,
    'To what extent is indigenous epistemology genuinely inaccessible to credentialed institutions (a structural incompatibility), versus being excluded by institutional unwillingness to engage (a gatekeeping choice that could be revoked)?',
    'Examination of collaborative research models where indigenous knowledge holders are positioned as co-investigators with institutional authority rather than consulted subjects. Assessment of whether such models produce co-authored scholarship that the academy recognizes as legitimate anthropological contribution.',
    'If genuinely inaccessible, no remedy short of institutional transformation will integrate indigenous frameworks. If excluded by choice, institutional policy changes could incorporate indigenous interpretation without displacing naturalism. The barrier type determines whether integration is theoretically possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_epistemology_accessibility, empirical, 'Whether the suppression of indigenous epistemology is structural incompatibility or political exclusion.').

omega_variable(
    funding_concentration_reversibility,
    'Is the concentration of research funding exclusively in naturalist frameworks (t=40, approaching near-monopoly) driven by the methodological superiority of naturalism, or by institutional incentive structures and path dependency that could be altered by redirecting funding?',
    'Policy experiments redirecting a fraction of research funding to explicitly non-naturalist frameworks (creationist paleontology, indigenous-led archaeological projects) and measuring output quality, citation patterns, and institutional resistance.',
    'If driven by superiority, funding concentration is justified by results. If driven by path dependency, the concentration is political and reversible. This determines whether funding monopoly is a necessary feature of the constraint or an incidental amplifier.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(funding_concentration_reversibility, preference, 'Whether funding gatekeeping reflects methodological necessity or institutional choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(anth_tr_t8, anthropological_record__naturalist_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(anth_tr_t16, anthropological_record__naturalist_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(anth_tr_t24, anthropological_record__naturalist_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(anth_tr_t32, anthropological_record__naturalist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__naturalist_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(anth_be_t8, anthropological_record__naturalist_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(anth_be_t16, anthropological_record__naturalist_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(anth_be_t24, anthropological_record__naturalist_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(anth_be_t32, anthropological_record__naturalist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(anth_be_t40, anthropological_record__naturalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(anth_su_t8, anthropological_record__naturalist_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(anth_su_t16, anthropological_record__naturalist_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(anth_su_t24, anthropological_record__naturalist_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(anth_su_t32, anthropological_record__naturalist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(anth_su_t40, anthropological_record__naturalist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(anthropological_record__naturalist_reading, 0.08).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% The anthropological_record kernel decomposes into three structurally distinct constraints corresponding to three readings. Each reading instantiates different ε, different beneficiary/victim structures, and different extractiveness profiles. The naturalist_reading exhibits high extractiveness (0.68) via credentialing gatekeeping and active suppression of alternatives. The creationist_reading exhibits moderate extractiveness via marginalization and legal/curricular exclusion. The indigenous_epistemology_reading exhibits high extractiveness via colonial epistemic displacement and identity suppression. Each story is ε-invariant within its reading; reading differences produce constraint differences, not measurement ambiguity. The three stories are linked by network edges reflecting their competitive relationship: the naturalist reading forecloses creationist appeals to supernatural causation within a single institutional framework, but coexists_with indigenous readings in plural epistemic landscapes. Consult the sibling reading stories for the full constraint family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__naturalist_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
