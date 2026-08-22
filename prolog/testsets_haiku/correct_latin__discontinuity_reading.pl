% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Classical Latin as Canonical Standard (Discontinuity Reading)
 *   domain: intellectual/linguistic/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the discontinuity reading of the
 *   correct-Latin kernel: Classical Latin as the canonical standard is the
 *   form preserved in ancient texts; medieval Latin is corrupt deviation
 *   requiring external reconstruction from textual sources. This reading
 *   establishes a rupture between legitimate and illegitimate usage, declares
 *   medieval practice corrupt, and positions classical philologists as the
 *   gatekeepers of correct form. The constraint benefits those who control
 *   the reconstruction process and damages those whose living practice is
 *   declared invalid. The founding problem (recovery of pure Classical form)
 *   was live in the Renaissance; it is now contested: modern linguistics
 *   treats medieval Latin as legitimate evolution, not corruption, and asks
 *   whether the discontinuity reading's premise (that ancient form is the
 *   authoritative form) rests on method or on power.
 *
 * KEY AGENTS:
 *   - Classical philologists: institutional power, agenda-setting authority, high exit (can shift standards); benefit from the gatekeeping role
 *   - Humanist reformers: powerful, mobile exit; benefit from the prestige of recovering pure wisdom
 *   - Medieval clerics and scholastic theologians: moderate-to-organized power, identity-locked exit; payers bearing the cost of declared corruption
 *   - Textual authorities: institutional power, analytical exit; benefit from controlling the standard-bearing corpus
 *   - Student population: powerless, trapped exit; excluded from the conversation about legitimacy
 *   - Ecclesiastical institutions: institutional power, constrained exit; observer seat under pressure to choose alignment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.68).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.72).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Classical Latin as Canonical Standard (Discontinuity Reading)").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "intellectual/linguistic/institutional").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '2b292839-92b3-46fd-b686-b425c299806a').
narrative_ontology:cs_kernel_codification('2b292839-92b3-46fd-b686-b425c299806a', fixed_text).
narrative_ontology:cs_authority_grounding('2b292839-92b3-46fd-b686-b425c299806a', extraction).
narrative_ontology:cs_interpretation_layer_present('2b292839-92b3-46fd-b686-b425c299806a').
narrative_ontology:cs_reading_relation('2b292839-92b3-46fd-b686-b425c299806a', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b292839-92b3-46fd-b686-b425c299806a', correct_latin__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('2b292839-92b3-46fd-b686-b425c299806a', foundational, classical_form_is_authoritative_standard).
narrative_ontology:cs_axiom_status(classical_form_is_authoritative_standard, holdable).
narrative_ontology:cs_axiom_grounding('2b292839-92b3-46fd-b686-b425c299806a', classical_form_is_authoritative_standard, conventional).
narrative_ontology:cs_axiom('2b292839-92b3-46fd-b686-b425c299806a', foundational, medieval_forms_are_linguistic_corruption).
narrative_ontology:cs_axiom_status(medieval_forms_are_linguistic_corruption, holdable).
narrative_ontology:cs_axiom_grounding('2b292839-92b3-46fd-b686-b425c299806a', medieval_forms_are_linguistic_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('2b292839-92b3-46fd-b686-b425c299806a', pure_classical_recovery).
narrative_ontology:cs_drift_state('2b292839-92b3-46fd-b686-b425c299806a', contemporary_post_historical_linguistics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2b292839-92b3-46fd-b686-b425c299806a', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, humanist_reformers).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_clerics).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, scholastic_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, textual_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and defend the standard that correct Latin is the Classical form attested in ancient texts (Cicero, Livy, Virgil). They control the canonical editions, determine what counts as error, and define the reconstruction methodology from textual sources. They benefit from the authority this grants them: employment, reputation, institutional gatekeeping over Latin education. Their exit is high — they can relocate to different linguistic standards or abandon the authority claim entirely without losing professional standing.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Advocate for Classical Latin reconstruction as the intellectual core of renaissance learning. They benefit by claiming that direct return to Classical purity is possible and desirable — it positions them as restorers of genuine wisdom against medieval corruption. They have exit options: they can shift to other classical texts or languages without losing standing, and they control resources and institutional backing.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, humanist_reformers, beneficiary,
    powerful, biographical, mobile, continental).

% Use medieval Latin as the living language of liturgy, canon law, and monastic practice. Under the discontinuity reading, their linguistic practice is declared corrupt deviation. They cannot exit without abandoning their professional and spiritual identity — the practice IS the role. They bear the cost of being told their transmitted practice is error, that their texts contain corruption, and that legitimate Latin is something external to their living usage and must be learned anew from ancient models they do not use.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_clerics, payer,
    moderate, biographical, identity_locked, local).

% Have built a sophisticated intellectual tradition in medieval Latin. They use it for logical precision, theological argument, and knowledge transmission. Under the discontinuity reading, this entire tradition is indicted as linguistically corrupt. Exit for them means abandoning their textual corpus, retraining in Classical form, and accepting that centuries of scholastic work now sits outside the legitimate usage set. Their identity as scholastic thinkers is fused with medieval Latin practice.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, scholastic_theologians, payer,
    organized, generational, identity_locked, continental).

% Own and control the ancient texts that serve as the standard-bearing corpus. They interpret what those texts authoritatively say, produce the editions, establish the apparatus criticus, and mediate all access to the supposed original forms. They benefit from this gatekeeping: institutional prestige, resource allocation, authority over what counts as correct. Their exit is analytical — they can shift standards at will because they do not depend on any particular standard working.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, textual_authorities, beneficiary,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, textual_authorities, agenda_setter).

% Must learn Latin via whatever form the institution teaches. Under the discontinuity reading, they are being taught the reconstructed Classical standard. They have no voice in whether medieval or Classical form is legitimate, and their learning burden increases: they must master a form declared to be historical rather than one embedded in living monastic or clerical practice. They are excluded from the conversation about which standard is correct.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, student_population, excluded,
    powerless, immediate, trapped, local).

% Maintain liturgy, canon law, and theological education. They take input from the constraint's operation: if medieval Latin is declared corrupt, they must either reform their practice, accept they are perpetuating error, or reject the discontinuity reading. Their exit is constrained by the need to maintain continuity in religious practice.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, ecclesiastical_institutions, observer,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single canonical standard for Latin literacy and textual interpretation across diverse medieval institutions and practices, enabling scholars from different regions and traditions to read and reference a common authoritative corpus and compare their own Latin usage against a fixed model.
% TRANSFER_FUNCTION: Moves interpretive authority from medieval practitioners (whose living usage is declared invalid) to classical philologists and textual editors (who control the standard-bearing texts and reconstruction methodology). It also transfers professional prestige and institutional authority: those who master the Classical standard gain standing; those who defend medieval forms lose it.
% ABSENT_VOICES: Medieval scribes, living practitioners of medieval liturgical Latin, monks whose entire intellectual identity is in scholastic Latin — they would object that the standard declared from without contradicts their lived transmission, that corruption claims are unfounded, and that linguistic vitality is in use, not in ancient texts. They are excluded from adjudicating what counts as correct in their own practice.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, medieval Latin would retain its legitimacy within ecclesiastical and scholastic practice; the prestige shift toward Classical form would not occur; humanists would lose a key rhetorical device for their intellectual program; institutional control over Latin education would realign toward monastic and cathedral schools rather than the Renaissance academies and courts that championed Classical purity. The entire reconstruction program would be unmotivated.
% FOUNDING_PROBLEM: Renaissance scholars observed that medieval Latin differed from Classical Roman usage in grammar, vocabulary, and style. They posed the question: is this difference evolution (legitimate change) or corruption (deviation from the true form)? They sought to recover the original pure Classical form as a foundation for intellectual authority and linguistic precision.
% FOUNDING_PROBLEM_CORROBORATION: Philologists and humanists attest that textual recovery and Classical purity are necessary and possible. Medieval scholars and practitioners attest that their form is legitimate and that corruption claims rest on a false premise: that a fixed ancient form is more authoritative than living, transmitted practice. Modern historical linguistics, outside both benefiting parties, attests that medieval Latin is not corrupt but a different linguistic system that evolved from Classical Latin — it is not a degraded copy but a genuine development with its own rules and coherence.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 final) because the constraint transfers authority from living practitioners to external gatekeepers, and medieval forms are declared invalid in the legitimate-usage set regardless of their coherence. The measurement series shows extraction growing from 0.35 (early Renaissance, before the reading solidified) to 0.68 (modern era, when the Classical standard is institutionalized). Suppression is correspondingly high (0.72 final) because the constraint's persistence depends on actively excluding medieval forms from legitimate usage — medieval clerics cannot simply use their transmitted practice without being told they are in error. Theater rises sharply (0.25 to 0.58) because the constraint's justification (recovery of pure form) increasingly serves institutional gatekeeping rather than actual linguistic analysis — the performative activity (citing Classical authorities, producing editions) overshadows the coordination function (establishing a shared reference standard). The discontinuity reading frames this as textual reconstruction; the constraint operates as institutional authority capture.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute different type classifications for the classical philologist seat (beneficiary, high exit, institutional power → coordination-frame, lower χ) versus the medieval cleric seat (payer, identity-locked exit, moderate power → extraction-frame, high χ). The constraint is authored as tangled_rope from the full structure: it DOES solve a coordination problem (unified Latin standard) AND it DOES extract from those whose practice is declared invalid. The suppression is structural (medieval forms are actively excluded from the legitimate-usage set) and internalized (medieval practitioners have come to believe their own forms are corrupt). The perspectival gap is irreducible: no single frame captures both seats' actual experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists occupy the beneficiary seat (d near 0.1–0.2): they control the standard, define what counts as error, and gain institutional prestige. Medieval practitioners occupy the target seat (d near 0.85–0.95): their practice is declared corrupt, they cannot exit without abandoning their identity, and the reconstruction standard is imposed from outside their community. Humanist reformers are beneficiaries (0.15–0.25): they gain intellectual authority by positioning themselves as restorers. Ecclesiastical institutions sit near neutral (d ~ 0.5): they benefit from a unified Latin standard but suffer the institutional pressure to reform or defend their medieval practice. The divergence is structural: from the philologist seat, this is genuine coordination (establishing a canonical standard); from the medieval cleric seat, it is enforced corruption of their legitimate usage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (recovery of pure Classical form from ancient texts) was live in the Renaissance. It is now contested: modern historical linguistics treats medieval Latin as legitimate linguistic evolution, not corruption. The constraint persists (suppression = 0.72) despite this shift in the underlying problem, because institutional investment in the Classical standard and the gatekeeping role it grants has become the primary driver of persistence. The theater_ratio rise (0.25 → 0.58) models this: initial focus on actual textual recovery has shifted toward performing the authority of the Classical standard and defending institutional prestige. This is a mandatrophy signature: the constraint's mandate (recovery of pure form) has been superseded, but the constraint remains because the institutional beneficiaries profit from its maintenance. The discontinuity reading does not admit mandatrophy directly — it frames the Classical standard as eternally authoritative — but the metrics reveal the structural shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corruption_vs_evolution,
    'Is medieval Latin corruption (deviation from a pure Classical form) or legitimate linguistic evolution (development with its own coherence and rules)?',
    'Historical-comparative linguistics: treat medieval Latin as a linguistic system in its own right, analyze it against the principles of language change, and determine whether it exhibits the patterns of corruption (random degradation, loss of coherence) or evolution (systematic change, internal consistency). Modern scholarship since the 1970s has resolved this in favor of evolution.',
    'If corruption is the correct diagnosis, the discontinuity reading''s fundamental premise stands. If evolution, the reading''s authority is based on imposing an external standard rather than recovering truth — the constraint becomes a pure extraction mechanism dressed as correction. The type would shift from tangled_rope (mixed coordination and extraction) toward snare (pure extraction with coordination cover story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corruption_vs_evolution, empirical, 'Whether medieval Latin is linguistically corrupt or evolved.').

omega_variable(
    textual_authority_assumption,
    'Why should the form attested in ancient texts be the authoritative standard for correctness rather than the form transmitted through living practice?',
    'Philosophical analysis of how standards are established: what makes a form correct — attestation in privileged texts, coherence of the linguistic system, acceptance by competent speakers, institutional codification? Different epistemologies yield different answers.',
    'If textual attestation is not inherently more authoritative than living transmission, the discontinuity reading''s foundation is not discovery but choice — a reading that privileges certain texts over others. This exposes the constraint as institutionalized preference backed by power rather than truth recovery. The suppression mechanism (excluding medieval forms from the legitimate set) becomes harder to justify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_assumption, conceptual, 'Epistemological status of textual authority versus living transmission.').

omega_variable(
    suppression_mechanism_structural_internalized,
    'Is the exclusion of medieval forms from the legitimate-usage set structural (external barriers: institutional policy, curriculum design, gatekeeping by philologists) or internalized (medieval practitioners have come to believe their own forms are corrupt)?',
    'Historical and ethnographic examination: in contexts where the discontinuity reading is not institutionally enforced (some monasteries, some canon-law communities that maintained practice without exposure to humanist reform), how do practitioners treat medieval Latin? Do they continue to use it confidently or do they absorb the corruption narrative? Structural suppression can be removed by policy change; internalized suppression persists after barrier removal.',
    'If suppression is entirely structural, removing institutional enforcement might restore medieval forms to legitimacy. If internalized, medieval practitioners would need to unlearn the corruption narrative before their own practice would feel legitimate again — the constraint carries its own enforcement inside the heads of those it targets. A mixed mechanism (as is likely) means the effective suppression is higher than the structural measure alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_internalized, empirical, 'Structural vs. internalized suppression mechanism in the constraint''s operation.').

omega_variable(
    discontinuity_vs_hybrid_boundary,
    'Is the discontinuity reading logically foreclosed by the hybrid reading, or do they coexist as alternative institutional choices?',
    'Logical analysis: does acceptance of the hybrid reading (Classical form as authoritative, but medieval transmission as legitimate, with targeted correction) logically entail rejection of the discontinuity reading (medieval forms as corrupt)? Or are they simply different policies on the same kernel?',
    'If they coexist, the three readings form a genuinely contested kernel with no logical winner. If the hybrid reading forecloses the discontinuity reading, the discontinuity reading''s institutional persistence is pure rent-seeking by classical philologists who prefer the authority-concentrating arrangement. This would shift the type assessment toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_vs_hybrid_boundary, conceptual, 'Logical relationship between discontinuity and hybrid readings of the correct-Latin kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__discontinuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(corr_tr_t100, correct_latin__discontinuity_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement(corr_tr_t200, correct_latin__discontinuity_reading, theater_ratio, 200, 0.48).
narrative_ontology:measurement(corr_tr_t300, correct_latin__discontinuity_reading, theater_ratio, 300, 0.55).
narrative_ontology:measurement(corr_tr_t400, correct_latin__discontinuity_reading, theater_ratio, 400, 0.57).
narrative_ontology:measurement(corr_tr_t500, correct_latin__discontinuity_reading, theater_ratio, 500, 0.58).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__discontinuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(corr_be_t100, correct_latin__discontinuity_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(corr_be_t200, correct_latin__discontinuity_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(corr_be_t300, correct_latin__discontinuity_reading, base_extractiveness, 300, 0.65).
narrative_ontology:measurement(corr_be_t400, correct_latin__discontinuity_reading, base_extractiveness, 400, 0.67).
narrative_ontology:measurement(corr_be_t500, correct_latin__discontinuity_reading, base_extractiveness, 500, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__discontinuity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(corr_su_t100, correct_latin__discontinuity_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(corr_su_t200, correct_latin__discontinuity_reading, suppression_requirement, 200, 0.64).
narrative_ontology:measurement(corr_su_t300, correct_latin__discontinuity_reading, suppression_requirement, 300, 0.7).
narrative_ontology:measurement(corr_su_t400, correct_latin__discontinuity_reading, suppression_requirement, 400, 0.71).
narrative_ontology:measurement(corr_su_t500, correct_latin__discontinuity_reading, suppression_requirement, 500, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__discontinuity_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The correct_latin kernel decomposes into three constraint stories, each instantiating a different reading of what counts as correct Latin. The discontinuity_reading declares Classical form (from ancient texts) as the standard and medieval forms as corrupt deviation. The continuity_reading treats living medieval practice as the source of legitimacy. The hybrid_reading accepts Classical authority but permits medieval transmission with textual correction. These are not one constraint viewed from different angles — they have different ε values, different victim sets, different institutional beneficiaries, and incompatible beneficiary/victim structures. They coexist in contemporary scholarship as live positions held by different institutional communities. All three are linked via network.affects_constraints to document the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
