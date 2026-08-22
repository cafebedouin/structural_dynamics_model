% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual Form Preservation as Identity Continuity
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This constraint instantiates the symbol-survival reading of
 *   catastrophe-memory-survival: the claim is that Jewish identity and
 *   collective memory persist THROUGH ritual form participation and symbolic
 *   boundary-maintenance, NOT through instrumental knowledge transmission or
 *   adaptive practice. Under this reading, a secularized Jew who does not
 *   participate in the forms has not inherited survival continuity — they
 *   have inherited only ethnic history without the living experience that
 *   constitutes the community. Rabbinic authority benefits by maintaining
 *   gatekeeping over what counts as 'authentic' continuity; secularized and
 *   youth-outside-observant-communities bear the cost of either participating
 *   in forms they do not understand or accepting partial exclusion from
 *   identity. The constraint is CLAIMED as tangled-rope (coordination
 *   function + active enforcement of extraction) but metrics describe
 *   substantial extractiveness (0.68 at interval end) and growing theater
 *   (rising from 0.32 to 0.48) — the extractive share is growing relative to
 *   functional coordination.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: maintains interpretive control; defines ritual forms as non-negotiable; benefits from interpretive gatekeeping
 *   - secularized_diaspora_jews: experience coherence crisis between identity-claim and practice-requirement; bear cost through excluded access or coerced participation
 *   - youth_outside_observant_communities: face identity-lock without community fluency; exit would mean self-erasure
 *   - observant_practitioners: benefit from community coherence and meaning-transmission
 *   - secular_cultural_institutions: excluded from framing authority; could offer alternative transmission pathways
 *   - analytic_observer: examines whether form-invariance is necessary or contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.71).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual Form Preservation as Identity Continuity").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '8ee7707d-e39f-49cd-a4d7-fa6896cf946f').
narrative_ontology:cs_kernel_codification('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', fixed_text).
narrative_ontology:cs_authority_grounding('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', lineage).
narrative_ontology:cs_interpretation_layer_present('8ee7707d-e39f-49cd-a4d7-fa6896cf946f').
narrative_ontology:cs_reading_relation('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', foundational, ritual_form_invariance_constitutive).
narrative_ontology:cs_axiom_status(ritual_form_invariance_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', ritual_form_invariance_constitutive, conventional).
narrative_ontology:cs_axiom('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', foundational, symbolic_participation_is_transmission).
narrative_ontology:cs_axiom_status(symbolic_participation_is_transmission, holdable).
narrative_ontology:cs_axiom_grounding('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', symbolic_participation_is_transmission, deontological).
narrative_ontology:cs_reference_frame('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', post_catastrophe_diaspora_survival_through_portable_practice).
narrative_ontology:cs_drift_state('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', contemporary_voluntary_diaspora, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8ee7707d-e39f-49cd-a4d7-fa6896cf946f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_diaspora_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, youth_outside_observant_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, observant_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and interprets the corpus of ritual forms (liturgy, observance cycles, textual study). Defines which practices constitute Jewish continuity and assigns meaning to ritual gestures. Derives authority and institutional legitimacy from being the custodian of 'authentic' practice. Can revise interpretation but rarely alters the ritual forms themselves — the forms' stability IS the source of their authority.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    organized, generational, mobile, global).

% Experience the constraint as a coherence crisis: the ritual forms are held up as the 'essence' of Jewish identity, yet the forms themselves (in Orthodox reading) are non-negotiable, arcane, and disconnected from lived experience. To 'be Jewish' under this framing requires participation in practices whose meaning has been severed from secular life. They bear the cost of either performing rituals whose meaning they do not access or accepting partial exclusion from communal identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_diaspora_jews, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, secularized_diaspora_jews, observer).

% Are born into Jewish ethnic/cultural identity but raised outside intensive ritual observance. Face the constraint as a double bind: continuity is indexed to ritual participation, yet they lack fluency in the forms and the communities of practice that sustain them. Continuation of 'being Jewish' in this reading requires adopting practices that feel alien, or accepting that secularization means identity loss. Identity is constitutive (exit would mean self-erasure), so they cannot simply choose out; the constraint persists through identity-lock even when participation is costly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, youth_outside_observant_communities, payer,
    powerless, biographical, identity_locked, global).

% Benefit from the constraint as a coherence device: ritual participation sustains their community identity, marks group boundaries, and transmits collective memory. The forms are intelligible to them because they are embedded in communities of practice. They experience the ritual system as enabling, not extractive, and their continuation within it is supported by institutional structures (family transmission, yeshiva networks, observant neighborhoods).
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, observant_practitioners, beneficiary,
    organized, generational, constrained, regional).

% Museums, cultural organizations, and Jewish secular education systems could frame Jewish continuity in non-ritual registers (language, history, ethics, secular nationalism). They are excluded from this constraint's authority structure — the frame 'survival = ritual continuity' pre-empts competing framings of what constitutes Jewish identity persistence. They would argue for continuity through alternative transmission pathways but have limited standing within the authoritative community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secular_cultural_institutions, excluded,
    powerful, generational, mobile, global).

% Examines the constraint structure: whether 'ritual form continuity' is a genuine requirement for Jewish identity-survival, or a contingent historical choice now calcified into seeming necessity. Asks whether the boundary-marking function of ritual REQUIRES form-invariance or whether meaning-transmission could persist through adaptive practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, analytic_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual form preserves the symbolic boundary between Jewish and non-Jewish identity; repetition of the forms across generations maintains collective memory and group coherence through symbolic-experiential participation. The repeated cycles (daily prayers, weekly Sabbath, annual holiday sequences) create a shared temporal frame that binds dispersed communities and transmits identity-constitutive narratives.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual practitioners to the rabbinic/textual establishment. Practitioners must defer to expert readings of what the forms mean and why they matter; the forms' non-negotiability ensures that meaning-making stays within the authorized interpretive tradition rather than being re-invented by each generation or subgroup.
% ABSENT_VOICES: Secular Jewish cultural and educational institutions, which would argue that Jewish identity and collective memory survive through language, history, ethics, and secular political projects without requiring ritual participation. Women whose ritual roles have historically been marginalized or prescribed. Diaspora communities whose own ritual adaptations are treated as 'inauthentic' deviations. Interfaith families whose children navigate competing identity frames.
% DISAPPEARANCE_RATIONALE: If the constraint (ritual form continuity as the marker of Jewish survival) disappeared, Jewish identity would reorganize around alternative continuity markers — language, historical consciousness, ethical commitment, secular cultural participation — already visible in secular Jewish communities that maintain identity without intensive ritual observance. The disappearance would not end Jewish community, but it would end the equation of survival with ritual-form participation, and interpretive authority would disperse away from rabbinic gatekeeping.
% FOUNDING_PROBLEM: After catastrophic diaspora and persecution, Jewish collective identity faced erasure or fragmentation. Ritual forms provided a portable, repetitive, internally-consistent symbolic system that could maintain group boundaries and transmit core narratives even across separated communities and generations, without requiring shared territorial, political, or economic structures.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic and observant-community scholarship attests the founding problem remains live — diaspora, assimilation, and secularization continue to threaten identity continuity. Secular Jewish historians and sociologists attest that the founding problem has substantially shifted: territorial dispersion is no longer catastrophic (diaspora is voluntary and relatively safe in many contexts); the threat is not diaspora but secular acculturation and intermarriage. They argue that identity persistence no longer depends on ritual form-invariance and that the constraint now functions to concentrate interpretive authority rather than to solve a live survival problem.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers meaning-making authority from the individual or community to the rabbinic establishment. Practitioners cannot reinterpret the forms or adapt them to changing contexts — the forms' stability IS the source of their authority. The cost borne by secularized Jews and youth is the coherence crisis: they are told their identity depends on participation in practices they have no fluency in or connection to. Suppression (0.71) is nearly as high because the constraint depends on active defense of form-invariance against adaptive alternatives. Theater (0.48, rising from 0.32) reflects that an increasing share of observant practice is maintenance of the forms themselves rather than extraction of functional meaning — people keep doing the rituals to keep the tradition alive, not primarily because the rituals serve their immediate purposes. The measurement series shows extraction and theater both rising over 40 years while foundational-problem corroboration (attesting the founding problem is still live) declines outside the beneficiary set — a symptom of mandatrophy. The rising trajectory is consistent with the theater-ratio diagnostic: a constraint whose function has atrophied but persists theatrically through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The constraint should compute very differently from the observant-practitioner seat vs. the secularized-victim seat. From the observant seat: genuine coordination (shared meaning, boundary maintenance, community coherence) with manageable cost (behavioral observance is their practice). The constraint is a rope or even a mountain to them — it emerged from community necessity and persists as lived practice. From the secularized seat: the same structures operate as coercive extraction with minimal coordination benefit — the forms are presented as non-negotiable but their meaning is inaccessible, so participation feels like empty performance. The constraint is a snare to them — exclusion from the community if they do not participate, alienation from the forms if they do. The engine should compute different types from different power atoms and exit-option profiles; this gap is the structure itself, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority sits at near-full-beneficiary (d ≈ 0.1): they collect interpretive gatekeeping authority, their exit is mobile (they could reframe continuity without leaving), and they are the constraint's agenda-setter. Secularized diaspora Jews sit at near-full-target (d ≈ 0.9): they are the identified victims (bear the coherence cost), their exit is constrained (assimilation is an exit but costs identity), and they pay through exclusion or coerced participation. Youth outside observant communities sit even farther toward the target end (d ≈ 0.95) because their exit is identity-locked — they cannot leave without self-erasure; their only exit is assimilation, which is treated as loss. Observant practitioners sit near symmetric (d ≈ 0.45): they experience genuine coordination benefit (community meaning), genuine cost (behavioral constraint), and their continued participation is not coerced — they could leave and rejoin secular culture, though the cost would be identity loss. Secular cultural institutions are not classified on directionality because they are absent from the constraint structure — they are excluded rather than coordinated or extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint meets classic mandatrophy indicators: (1) founding_problem_status is contested, with external corroboration (secular historians, sociologists) attesting the problem has shifted (diaspora is now voluntary and relatively safe in many contexts, not catastrophic); (2) theater_ratio is rising (from 0.32 to 0.48) — an increasing share of observant activity is maintenance-of-form itself rather than extraction of meaning; (3) secularized Jews maintain Jewish identity and collective memory WITHOUT ritual participation (secular Yiddish culture, Jewish historical societies, ethical movements, Israeli secular nationalism), proving the founding problem has alternate solutions; (4) rabbinic authority is increasingly maintained through social reinforcement and institutional inertia rather than through solving a live coordination problem. The constraint persists, but the mandate (the founding problem it was built to solve) has largely been resolved by changed historical conditions (safe diaspora, voluntary community membership). A true mandatrophy resolution would require either: (a) acknowledging that ritual form continuity is ONE OPTION for identity transmission, not the ONLY option, which would dissolve rabbinic gatekeeping authority; or (b) reframing the founding problem from 'survival despite catastrophic diaspora' to 'boundary maintenance against assimilation', which shifts the justification but does not resolve the core extraction (the secularized Jews would still face the same coherence cost).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_instrumental_transmission,
    'Does Jewish identity continuity actually REQUIRE symbolic-form participation, or can it persist through alternative transmission pathways (language, historical consciousness, ethical commitment, secular cultural practice)?',
    'Comparative study of secular Jewish communities: do generations of secular Jews maintain Jewish identity and collective memory without intensive ritual observance? Does their identity feel ''continuous'' to them? Do their communities transmit distinctly Jewish values and narratives? Evidence from Israel (secular majority), Yiddish culture movements, Jewish historical societies, and secular Jewish ethical movements would test whether the constraint''s framing (survival = ritual continuity) is necessary or contingent.',
    'If identity and continuity persist without ritual participation, the constraint is revealed as extractive gatekeeper (rabbinic authority maintains authority through monopolizing continuity-narratives) rather than as genuine coordination. If they do not persist, the constraint''s claim to necessary function is vindicated. The test has high stakes for mandatrophy classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(symbolic_vs_instrumental_transmission, empirical, 'Whether ritual-form participation is necessary or contingent for Jewish identity persistence.').

omega_variable(
    identity_lock_mechanism,
    'For youth outside observant communities, is the measured suppression (0.71) structural (economic dependency on observant community, legal barriers to alternative identity-paths) or internalized (the youth believes they DESERVE exclusion, has fused their self-concept with the community''s judgment, or has limited reality-testing contact with secular Jewish alternatives)?',
    'Post-exit suppression trajectory: track youth who leave observant communities. If suppression persists after the extractive structure is removed (they continue to feel they should be participating, feel shame, maintain surveillance of their own non-compliance even after leaving), suppression is substantially internalized. If suppression ceases after structural exit, it was primarily structural.',
    'If internalized: the constraint''s effective suppression is higher than the structural measure suggests — the target carries it after exit, and repair requires identity-frame dissolution. If structural: exit would relieve suppression, suggesting identity-lock is not absolute. The classification implication shifts from snare (inescapable) toward constrained or mobile exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock suppression is structural or internalized.').

omega_variable(
    reading_boundary_foreclosure,
    'Does the symbol-survival reading''s core premise (survival IS continuity of practice; form-participation IS the transmission) logically foreclose the hybrid-encoding reading (which posits BOTH symbolic AND practical content, with survival depending on both), or do they represent genuinely incompatible framework-level commitments versus different emphases within a compatible framework?',
    'Formal analysis of reading-premises: does the symbol-reading require that practical knowledge is NOT transmitted through ritual (so hybrid encoding would contradict it), or merely that symbolic participation is NECESSARY (so hybrid encoding would be compatible, just adding more content)? Interview rabbinic and secular scholars who hold each reading to determine whether they perceive logical contradiction or emphasis-difference.',
    'If they foreclose: reading_relations should list ''forecloses'' for the hybrid_encoding sibling. If they are compatible: reading_relations should list ''coexists_with''. The classification affects how the engine models kernel-reading dynamics: foreclosure patterns suggest framework-level contests; coexistence suggests different communities of interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_foreclosure, conceptual, 'Whether symbol-survival and hybrid-encoding readings are logically exclusive or emphasis-variant.').

omega_variable(
    mandatrophy_vs_authentic_renewal,
    'Is the rising theater_ratio (0.32 → 0.48) evidence of mandatrophy (the founding problem has been solved and the constraint persists through inertia), or evidence of authentic renewal (the meaning of the ritual practice has evolved, and observant practitioners perform the forms precisely to renew and sustain their meaning for new generations)?',
    'Ethnographic interviews with observant practitioners: do they experience their practice as rote maintenance (theater indicator) or as meaningful renewal? Does the content of ritual interpretation (rabbinic commentary, community discourse about the rituals) suggest substantive meaning-making or form-preservation-for-its-own-sake? Compare practitioners'' reported meaning-experience with external analysis of what functional work the rituals accomplish.',
    'If authentic renewal: the theater_ratio is a measurement error and the constraint should be reclassified upward in coordination function. If mandatrophy: the theater_ratio confirms that maintenance-of-form has become the primary function, the founding problem has shifted, and the constraint is extractive gatekeeper more than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_vs_authentic_renewal, empirical, 'Whether rising theater signals mandatrophy or authentic renewal of practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cata_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__symbol_survival_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of catastrophe_memory_survival kernel. The competence_transmission_reading locates survival in practical knowledge encoded in ritual; the hybrid_encoding_reading claims both symbolic and practical transmission. This reading (symbol_survival) locates survival in symbolic-form participation itself. All three share the kernel commitment to Jewish identity and collective memory but decompose it into structurally distinct constraints with different beneficiary/victim structures, different ε values, and different mandatrophy profiles. The sibling constraints are linked via network.affects_constraints so the corpus can model the reading-contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
