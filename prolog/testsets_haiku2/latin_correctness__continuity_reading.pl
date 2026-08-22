% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Medieval Latin as Legitimate Linguistic Continuation
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   Medieval Latin is one reading of a contested kernel about what makes
 *   Latin 'correct' or 'legitimate.' Under the continuity reading
 *   instantiated here, Medieval Latin is treated as the natural, organic
 *   continuation of classical Latin through linguistic change — not
 *   corruption, not a separate language, but legitimate descent. The
 *   constraint coordinates a community (medieval scribes, ecclesiastical
 *   institutions, Latin-readers) around the claim that innovations in
 *   phonology, morphology, and vocabulary are the expected products of
 *   language transmission, not departures from an external standard. The
 *   constraint is low-extractiveness under this reading because no victim set
 *   exists: medieval users inherit legitimately, the Church administers a
 *   real coordination function (unified liturgical and administrative
 *   language), and the relationship between classical tradition and medieval
 *   practice is presented as natural descent. The rupture_reading (Latin is a
 *   fixed textual standard requiring reconstruction) and the hybrid_reading
 *   (classical for literary, medieval for technical) are sibling constraint
 *   stories within the same kernel contest. This reading does not claim to be
 *   unique or natural — it claims to be one coherent framing among competing
 *   framings.
 *
 * KEY AGENTS:
 *   - Medieval scribes and scholars: inherit classical training, speak evolved Latin, claim legitimacy for their innovations as natural descent
 *   - Ecclesiastical institutions: exercise agenda-setting authority over Latin literacy and liturgical standardization, benefit from a reading that permits adaptation
 *   - Classical philologists: sit outside the medieval constraint, measure against ancient attested norms, have arbitrage power in modern academic adjudication
 *   - Medieval non-literates: excluded from the dispute, but their speech patterns are the phonological pressure the constraint accommodates as 'natural'
 *   - Competing readings (rupture, hybrid): non-agents but structural rivals for the authority to define Latin legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.18).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.22).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Medieval Latin as Legitimate Linguistic Continuation").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "historical_linguistics/intellectual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, '475806fa-67cf-4f49-8cfb-d125794dfe7d').
narrative_ontology:cs_kernel_codification('475806fa-67cf-4f49-8cfb-d125794dfe7d', fixed_text).
narrative_ontology:cs_authority_grounding('475806fa-67cf-4f49-8cfb-d125794dfe7d', lineage).
narrative_ontology:cs_interpretation_layer_present('475806fa-67cf-4f49-8cfb-d125794dfe7d').
narrative_ontology:cs_reading_relation('475806fa-67cf-4f49-8cfb-d125794dfe7d', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('475806fa-67cf-4f49-8cfb-d125794dfe7d', latin_correctness__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('475806fa-67cf-4f49-8cfb-d125794dfe7d', foundational, medieval_innovations_natural_descent).
narrative_ontology:cs_axiom_status(medieval_innovations_natural_descent, holdable).
narrative_ontology:cs_axiom_grounding('475806fa-67cf-4f49-8cfb-d125794dfe7d', medieval_innovations_natural_descent, empirically_contingent).
narrative_ontology:cs_axiom('475806fa-67cf-4f49-8cfb-d125794dfe7d', foundational, transmission_continuity_confers_legitimacy).
narrative_ontology:cs_axiom_status(transmission_continuity_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('475806fa-67cf-4f49-8cfb-d125794dfe7d', transmission_continuity_confers_legitimacy, conventional).
narrative_ontology:cs_reference_frame('475806fa-67cf-4f49-8cfb-d125794dfe7d', classical_latin_living_transmission).
narrative_ontology:cs_drift_state('475806fa-67cf-4f49-8cfb-d125794dfe7d', high_medieval_period, gap(stable, minor, true)).
narrative_ontology:cs_created_at('475806fa-67cf-4f49-8cfb-d125794dfe7d', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_scribes_and_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, ecclesiastical_institutions).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, organic_language_change).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, legitimacy_through_descent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Medieval Latin-users who inherit and transmit classical Latin texts while adapting them to contemporary speech patterns, phonology, and vocabulary. They benefit from a reading that legitimizes their innovations as natural linguistic descent rather than corruption or failure. Their exit from the constraint would mean abandoning any claim to continuity with the classical tradition and accepting the status of 'barbarous' outsiders.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_scribes_and_scholars, beneficiary,
    moderate, biographical, constrained, regional).

% The Church preserves and teaches Latin, conducts liturgy in it, and enforces liturgical correctness. Under the continuity reading, the Church can adapt the language to pastoral and administrative needs while maintaining that it remains legitimate Latin, not a foreign tongue. The Church's institutional authority over textual transmission and education gives it power to enforce which reading of Latin legitimacy predominates in practice.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, mobile, regional).

% Specialized scholars who study ancient texts and reconstruct classical norms. They sit outside the constraint as it operates in medieval communities; they measure against an external standard (attested classical usage) and have the exit option of treating medieval forms as a separate object of study rather than as corrupted Latin. Their professional authority gives them leverage in how competing readings of Latin legitimacy are adjudicated in the modern academy.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, classical_philologists, observer,
    powerful, biographical, arbitrage, global).

% Speakers of the Romance vernaculars and other illiterate peoples who have no voice in the dispute over what counts as legitimate Latin. They would have claims on the language as it evolved into their vernaculars, but they are excluded from the textual and scholarly conversation where the readings of Latin legitimacy are adjudicated. Any reading adopted shapes what institutional resources flow to literacy and language standardization in their communities.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_non_literates, excluded,
    powerless, immediate, trapped, local).

% The set of alternative framings (rupture_reading, hybrid_reading) that compete with this continuity reading for authority. They are listed as a non-agent entity to establish the doctrinal disagreement as part of the constraint's landscape.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, competing_normative_readings, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(latin_correctness__continuity_reading, competing_normative_readings).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(latin_correctness__continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared linguistic framework for ecclesiastical, administrative, and intellectual communication across a geographically dispersed medieval Latin-reading community. The constraint coordinates on the claim that medieval innovations are legitimate descendants of classical forms rather than departures from an external standard, allowing users to adapt the language to contemporary needs while maintaining a unified identity of 'Latin-users' rather than splintering into acknowledged separate languages.
% TRANSFER_FUNCTION: The constraint transfers authority and interpretive legitimacy from classical-text standards to the living medieval practice. Scribes and scholars who inherit classical training gain the right to speak of their own linguistic choices as 'Latin' rather than as 'corruptions of' or 'departures from' Latin. The Church gains the ability to standardize liturgical and administrative Latin at a regional or institutional level without defending every departure as an exception to an external rule.
% ABSENT_VOICES: Medieval non-literates and speakers of emerging Romance vernaculars are structurally excluded from the dispute over what counts as legitimate Latin. They would have strong claims on the language as it evolved under their speech patterns, but their voices appear only as implied force in the constraint (the phonological and vocabulary changes the reading must accommodate as 'natural'), not as participants in the textual and scholarly conversation. Literary and rhetorical purists who might prefer the rupture_reading are also partially excluded: their objections to continuity are heard only through rare monastic variants or later scholarly tradition, not as dominant medieval voices.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the rupture_reading became canonical (Latin is a fixed ancient standard requiring reconstruction), medieval scribes would need to choose between abandoning their linguistic heritage as illegitimate or creating an acknowledged separate language family. The ecclesiastical literacy infrastructure would face pressure to either teach 'Classical Latin' as a foreign language (with the attendant cost) or to explicitly relabel medieval Latin as a vernacular Romance language. The coordination function would be lost: instead of a single 'Latin' tradition spanning classical antiquity through the medieval period, there would be a classical standard and a medieval practice with no bridge.
% FOUNDING_PROBLEM: After the collapse of the Western Roman Empire, Latin-users in the medieval West inherited classical texts and traditions but spoke a language that was evolving under the pressure of vernacular phonology, new vocabulary, and simplified inflectional morphology. The founding problem is how to maintain legitimate Latin literacy and continuity with the classical tradition while the living language changes.
% FOUNDING_PROBLEM_CORROBORATION: Medieval ecclesiastical authorities and scribes themselves attest the founding problem: they explicitly claim to be writing Latin (not a new language) while acknowledging their language differs from the classical texts they read. Modern historical linguists who study medieval Latin as a living system (rather than as corrupted classical Latin) — such as those in the tradition of Roger Wright and scholars of ecclesiastical Latin — attest that the founding problem remained live throughout the medieval period: the tension between the fixed textual tradition and the evolving spoken/written practice.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__continuity_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).
:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the continuity reading treats medieval innovations as natural, not coerced. There are no victims — medieval users are not forced to abandon a language they prefer; they inherit and adapt one they speak naturally. Suppression is low (0.22) because the constraint does not suppress alternatives; it coordinates on a claim about what the alternatives ARE (legitimate descendants, not corruptions). Theater is low (0.12) because the constraint's function is genuine: it enables a geographically dispersed Latin-reading community to maintain a unified linguistic identity while accommodating natural variation. The measurements show stability across the medieval interval (500–1200): the constraint's extractiveness and suppression remain low and flat. The slight rise from 500 to 950 reflects increasing institutionalization of the Church's role in standardizing liturgical and administrative Latin, then plateaus as the constraint stabilizes. By 1200, the constraint is well-established: medieval Latin is treated as legitimate throughout the Latin-reading world, even as individual scribes and regions innovate. Accessibility_collapse is moderate (0.45): once one knows the constraint exists (that medieval Latin is treated as legitimate continuation), one cannot simply declare medieval forms as 'wrong' without engaging the framework. But the constraint is not physically inevitable like a mountain would be — communities could adopt the rupture_reading and treat medieval forms as corruption, and resistance to the continuity reading is substantial (0.58) from classical philologists and communities committed to textual purism. The claim/metric independence is maintained: the constraint is CLAIMED as rope (coordination without major extraction) and the metrics describe low extractiveness and suppression; the engine computes whether this is structurally true.
 *
 * PERSPECTIVAL GAP:
 *   The continuity reading itself is agreed upon within the medieval Latin-reading community: both ecclesiastical and secular scribes claim to write Latin, not a new language. The perspectival gap opens between THIS reading and the sibling readings (rupture and hybrid), not within medieval communities. From a rupture_reading perspective (classical Latin is a fixed textual standard), medieval scribes are degrading the language and the constraint is a false mountain — beneficiaries claiming naturalness for a constructed standard. From a hybrid_reading perspective, the constraint is partially overstated — classical norms apply in literary domains while medieval forms are legitimate in technical/practical domains. The continuity_reading itself predicts no seat divergence because it treats medieval users as legitimate inheritors across all domains.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval scribes and ecclesiastical institutions are the structural beneficiaries because the continuity reading legitimizes their linguistic choices. Classical philologists are analysts who sit outside the medieval constraint's operation and gain professional authority from maintaining the classical textual standard as a reference point. Medieval non-literates are excluded from the decision-making but their speech patterns are the force that drives the phonological and vocabulary changes the constraint must accommodate. No seat experiences this constraint as pure extraction because the constraint is coordinative: it enables a community to maintain a shared language identity while accommodating natural linguistic change.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE: the tension between fixed classical texts and evolving medieval speech patterns persists throughout the medieval period. The continuity reading DOES solve the founding problem by treating evolution as legitimate descent rather than corruption, which is why the constraint remains stable across the interval. There is no sign of mandatrophy (a constraint whose founding problem has died but which persists). The theater_ratio stays low because the constraint performs a real coordination function. If the founding problem were dead (if medieval Latin had fully merged with the vernacular Romance languages and lost its connection to classical literacy), the constraint would begin to shift toward theater — maintenance would become purely performative, a vestigial claim of legitimacy. But the medieval evidence shows the founding problem remains live: scribes and scholars continuously grapple with the tension between classical forms and contemporary speech, and the continuity reading provides a coherent way to honor both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_premise,
    'Is medieval Latin a legitimate continuation of classical Latin through organic linguistic change, or is it a degradation of a fixed textual standard requiring reconstruction from ancient sources?',
    'This is a definitional/conceptual disagreement rather than an empirical one. Resolution would come through acceptance of a framework for what counts as ''legitimate'' linguistic descent: does legitimacy attach to historical continuity and community transmission (continuity reading) or to adherence to attested classical norms (rupture reading)? The disagreement is foundational, not empirically resolvable.',
    'If resolved toward continuity, medieval Latin users are legitimate inheritors with no victims; extractiveness and suppression remain low. If resolved toward rupture, medieval forms become corruptions, the constraint becomes a false mountain (beneficiaries claiming naturalness for a constructed standard), and extractiveness rises sharply as the constraint begins to suppress non-classical forms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_vs_rupture_premise, conceptual, 'The core reading disagreement: is legitimacy grounded in historical descent or in adherence to a fixed standard?').

omega_variable(
    vernacular_phonology_naturalness,
    'Are the phonological changes visible in medieval Latin (loss of final syllables, vowel shifts, palatalization) organic natural changes inherent to the language''s transmission, or are they corruptions introduced by contact with vernacular Romance speech?',
    'Historical phonology and comparative Romance linguistics: if the changes are systematic, follow predictable phonological laws, and occur independently in multiple Romance-language regions, they are better explained as natural drift. If they correlate perfectly with externally imposed contacts and represent departures from classical rules without internal phonological motivation, the corruption reading is stronger.',
    'If vernacular changes are natural, the constraint''s legitimacy is secure and extractiveness stays low. If they are contamination from outside, the constraint begins to shift toward suppressing non-classical forms as external impositions rather than legitimate evolution, raising extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_phonology_naturalness, empirical, 'Whether medieval phonological change is organic drift or external corruption.').

omega_variable(
    transmission_authority_continuity,
    'Did medieval Latin-users genuinely inherit and transmit classical texts and training, or did the break in Roman institutions mean they were reconstructing or reinterpreting an external tradition?',
    'Historical study of monastic and clerical transmission chains, manuscript traditions, evidence of direct classical education in late antiquity and early medieval periods. If continuous institutional transmission can be documented, the continuity reading is supported; if there are breaks and later reconstruction from written sources without continuous community practice, the rupture reading is stronger.',
    'Continuous transmission strengthens the legitimacy of calling medieval forms ''continuations'' because the community''s own practice is the ground. Reconstructive/reinterpreted transmission means medieval users are claiming descent from a tradition they only partially access directly, which shifts the reading toward a constructed claim rather than a natural fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_authority_continuity, empirical, 'Whether medieval Latin transmission was continuous institutional practice or reconstructed study of texts.').

omega_variable(
    hybrid_reading_institutional_split,
    'Does the hybrid_reading (classical norms for literary/rhetorical domains; medieval forms legitimate for technical/practical domains) represent a live alternative in medieval practice, or is it a modern analytical imposition?',
    'Textual and manuscript analysis: careful study of whether medieval scholars explicitly differentiated their register and style depending on domain, or whether the distinction is a modern philologist''s imposition on a more fluid actual practice.',
    'If the hybrid reading reflects actual medieval practice divisions, the continuity reading is overstated and should be seen as domain-specific (continuity in technical/practical writing, stylistic choice in literary domains). If the hybrid is analytical, the continuity reading better captures the actual medieval stance of legitimacy across all domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_institutional_split, empirical, 'Whether medieval practice actually supports a register-divided hybrid reading.').

omega_variable(
    committer_kernel_contest,
    'This constraint is one reading of the contested kernel ''latin_correctness''. What structural properties distinguish the continuity_reading from its siblings rupture_reading and hybrid_reading?',
    'The disagreement is NOT empirically resolvable: it is a dispute over which FRAMEWORK (historical descent vs. textual standard vs. domain-divided hybridity) should define what counts as legitimate Latin. The sibling readings coexist as live positions held by different parties in the modern academy. No amount of historical evidence will settle the framework question itself, though evidence can support or undermine claims WITHIN each framework.',
    'The continuity_reading asserts that medieval Latin is legitimate because it is the historical continuation of the classical language, transmission-grounded, not evaluation-grounded. The rupture_reading asserts that legitimacy is defined by adherence to classical textual norms. The hybrid_reading splits the difference by domain. No reading forecloses the others within the larger commitment system of ''Latin correctness'' — they coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_contest, conceptual, 'The kernel contest: which framework defines Latin legitimacy — historical descent, textual adherence, or domain-divided hybridity?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 500, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t500, latin_correctness__continuity_reading, theater_ratio, 500, 0.08).
narrative_ontology:measurement_basis(lati_tr_t500, projected).
narrative_ontology:measurement(lati_tr_t650, latin_correctness__continuity_reading, theater_ratio, 650, 0.1).
narrative_ontology:measurement_basis(lati_tr_t650, projected).
narrative_ontology:measurement(lati_tr_t800, latin_correctness__continuity_reading, theater_ratio, 800, 0.12).
narrative_ontology:measurement_basis(lati_tr_t800, observed).
narrative_ontology:measurement(lati_tr_t950, latin_correctness__continuity_reading, theater_ratio, 950, 0.13).
narrative_ontology:measurement_basis(lati_tr_t950, observed).
narrative_ontology:measurement(lati_tr_t1100, latin_correctness__continuity_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement_basis(lati_tr_t1100, observed).
narrative_ontology:measurement(lati_tr_t1200, latin_correctness__continuity_reading, theater_ratio, 1200, 0.12).
narrative_ontology:measurement_basis(lati_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t500, latin_correctness__continuity_reading, base_extractiveness, 500, 0.15).
narrative_ontology:measurement_basis(lati_be_t500, projected).
narrative_ontology:measurement(lati_be_t650, latin_correctness__continuity_reading, base_extractiveness, 650, 0.16).
narrative_ontology:measurement_basis(lati_be_t650, projected).
narrative_ontology:measurement(lati_be_t800, latin_correctness__continuity_reading, base_extractiveness, 800, 0.18).
narrative_ontology:measurement_basis(lati_be_t800, observed).
narrative_ontology:measurement(lati_be_t950, latin_correctness__continuity_reading, base_extractiveness, 950, 0.19).
narrative_ontology:measurement_basis(lati_be_t950, observed).
narrative_ontology:measurement(lati_be_t1100, latin_correctness__continuity_reading, base_extractiveness, 1100, 0.18).
narrative_ontology:measurement_basis(lati_be_t1100, observed).
narrative_ontology:measurement(lati_be_t1200, latin_correctness__continuity_reading, base_extractiveness, 1200, 0.18).
narrative_ontology:measurement_basis(lati_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t500, latin_correctness__continuity_reading, suppression_requirement, 500, 0.18).
narrative_ontology:measurement_basis(lati_su_t500, projected).
narrative_ontology:measurement(lati_su_t650, latin_correctness__continuity_reading, suppression_requirement, 650, 0.2).
narrative_ontology:measurement_basis(lati_su_t650, projected).
narrative_ontology:measurement(lati_su_t800, latin_correctness__continuity_reading, suppression_requirement, 800, 0.22).
narrative_ontology:measurement_basis(lati_su_t800, observed).
narrative_ontology:measurement(lati_su_t950, latin_correctness__continuity_reading, suppression_requirement, 950, 0.23).
narrative_ontology:measurement_basis(lati_su_t950, observed).
narrative_ontology:measurement(lati_su_t1100, latin_correctness__continuity_reading, suppression_requirement, 1100, 0.22).
narrative_ontology:measurement_basis(lati_su_t1100, observed).
narrative_ontology:measurement(lati_su_t1200, latin_correctness__continuity_reading, suppression_requirement, 1200, 0.22).
narrative_ontology:measurement_basis(lati_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__continuity_reading, 0.05).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel decomposes into three structurally distinct constraint stories: continuity_reading (this story), rupture_reading, and hybrid_reading. Each reading instantiates a different framework for what defines Latin legitimacy and produces different ε values, victim structures, and types. The continuity_reading treats medieval evolution as natural descent (low extraction, no victims, rope classification). The rupture_reading treats medieval forms as corruption of a fixed standard (higher extraction, medieval users as victims, tangled_rope or snare classification). The hybrid_reading splits the difference by domain, treating classical norms as binding only in literary/rhetorical contexts (moderate extraction, technical-domain users as legitimate, tangled_rope classification). These are not three measurements of the same constraint; they are three constraints instantiated by different readings of the same kernel. The network edges establish that all three affect each other: the continuity_reading's success in establishing medieval legitimacy constrains the resources and cultural authority available to the rupture_reading, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
