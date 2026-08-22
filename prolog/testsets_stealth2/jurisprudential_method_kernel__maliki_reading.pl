% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Medinan Living Practice ('Amal Ahl al-Madina) as Independent Source of Law
 *   domain: religious/legal/institutional-history
 *
 * SUMMARY:
 *   Within the shared commitment that law derives from the Qur'an and the
 *   Prophet's Sunna, the Maliki reading holds that the continuous practice of
 *   the Medinan community ('amal ahl al-Madina) is itself a valid source of
 *   law — able to confirm, supply, and where necessary override isolated
 *   transmitted reports — because Medina's unbroken communal life is held to
 *   preserve the Prophet's practice more faithfully than any chain of
 *   individual narrators. This file instantiates ONE reading of the
 *   jurisprudential_method_kernel; the Hanafi, Shafi'i, and Hanbali readings
 *   are separate constraint stories linked through the network section, each
 *   with its own epsilon and beneficiary/victim structure. The epsilon
 *   referent is the Medinan-practice arrangement itself — the standing
 *   arrangement under contest — described as it operates, not as its rivals
 *   characterize it. KEY AGENTS (by structural relationship): -
 *   medinan_scholarly_lineage: agenda-setting custodian and principal
 *   beneficiary (institutional / identity_locked) — administers the
 *   practice-based source and collects the authority it generates; -
 *   maliki_school_jurists: working beneficiaries (organized /
 *   identity_locked) — exercise the method professionally across the school's
 *   territories; - hadith_transmission_specialists: principal payers
 *   (organized / constrained) — their grading craft authenticates but does
 *   not arbitrate where practice overrides reports; - rival_school_jurists:
 *   payers (organized / mobile) — non-Medinan methods subordinated wherever
 *   the Medinan method governs; - lay_muslim_communities: incidental
 *   beneficiaries (moderate / constrained); - zahiri_text_only_critics:
 *   excluded objectors (organized / mobile); - comparative_fiqh_historians:
 *   analytical observers (analytical / analytical).
 *
 * KEY AGENTS:
 *   - medinan_scholarly_lineage: agenda-setting custodian and principal beneficiary (institutional / identity_locked)
 *   - maliki_school_jurists: working beneficiaries of the method (organized / identity_locked)
 *   - hadith_transmission_specialists: principal payers whose craft is subordinated (organized / constrained)
 *   - rival_school_jurists: payers holding non-Medinan methods (organized / mobile)
 *   - lay_muslim_communities: incidental beneficiaries receiving stable law (moderate / constrained)
 *   - zahiri_text_only_critics: excluded objectors outside the school's councils (organized / mobile)
 *   - comparative_fiqh_historians: analytical observers (analytical / analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.55).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.5).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Medinan Living Practice ('Amal Ahl al-Madina) as Independent Source of Law").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "religious/legal/institutional-history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '01d913f6-7235-4ca0-82d0-b7933e239468').
narrative_ontology:cs_kernel_codification('01d913f6-7235-4ca0-82d0-b7933e239468', fixed_text).
narrative_ontology:cs_authority_grounding('01d913f6-7235-4ca0-82d0-b7933e239468', lineage).
narrative_ontology:cs_interpretation_layer_present('01d913f6-7235-4ca0-82d0-b7933e239468').
narrative_ontology:cs_reading_relation('01d913f6-7235-4ca0-82d0-b7933e239468', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('01d913f6-7235-4ca0-82d0-b7933e239468', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_reading_relation('01d913f6-7235-4ca0-82d0-b7933e239468', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('01d913f6-7235-4ca0-82d0-b7933e239468', foundational, prophetic_practice_preserved_in_medinan_amal).
narrative_ontology:cs_axiom_status(prophetic_practice_preserved_in_medinan_amal, holdable).
narrative_ontology:cs_axiom_grounding('01d913f6-7235-4ca0-82d0-b7933e239468', prophetic_practice_preserved_in_medinan_amal, empirically_contingent).
narrative_ontology:cs_axiom('01d913f6-7235-4ca0-82d0-b7933e239468', secondary, communal_continuity_outweighs_isnad_chains).
narrative_ontology:cs_axiom_status(communal_continuity_outweighs_isnad_chains, holdable).
narrative_ontology:cs_axiom_grounding('01d913f6-7235-4ca0-82d0-b7933e239468', communal_continuity_outweighs_isnad_chains, empirically_contingent).
narrative_ontology:cs_reference_frame('01d913f6-7235-4ca0-82d0-b7933e239468', medina_living_witness_of_prophetic_practice).
narrative_ontology:cs_drift_state('01d913f6-7235-4ca0-82d0-b7933e239468', post_critical_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('01d913f6-7235-4ca0-82d0-b7933e239468', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_school_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, hadith_transmission_specialists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, rival_school_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, lay_muslim_communities).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, continuity_superiority_doctrine).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, medinan_custodial_authority_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches, transmits, and adjudicates according to the settled practice of the Medinan community as received from the companions' generation. When transmitted reports conflict or fall silent, the community's continuous practice supplies the ruling. Teaching posts, judicial influence, and custodial authority flow to holders of this lineage; leaving it would mean renouncing the claim that their city's practice is the Prophet's preserved practice — the claim on which their entire standing rests.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, beneficiary).

% Decide cases and write responsa across North Africa, al-Andalus, and later West Africa within the Medinan-practice method. The method lets them resolve conflicts between reports by appealing to continuous communal practice, giving their rulings a stability that purely narration-based methods lack. Their professional standing is bound to the school's methodological identity; moving to another school means retraining and loss of position.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_school_jurists, beneficiary,
    organized, generational, identity_locked, continental).

% Collect, grade, and transmit reports through chains of named narrators. Where the Medinan method lets continuous practice override an apparently sound report, their grading work loses final say: their craft authenticates but does not arbitrate. Their expertise travels across regions, but abandoning transmission work altogether is not a live option for people trained into it from youth.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, hadith_transmission_specialists, payer,
    organized, biographical, constrained, global).

% Jurists of the Kufan analogical tradition, the Shafi'i hierarchical method, and the text-literalist tradition. In territories where the Medinan-practice method governs courts and teaching, their claims that their own methods carry equal authenticity are subordinated, costing them standing and institutional access there. They retain full professional standing in their home regions, which keeps exit realistic and their costs geographically bounded.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, rival_school_jurists, payer,
    organized, generational, mobile, global).

% Ordinary believers in Medinan-method territories receive a stable body of law anchored in visible communal practice rather than in contested chains of narration, and can see the practice their law rests on. They also absorb costs where the practices of their own localities are ranked below Medinan precedent in disputed questions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, lay_muslim_communities, beneficiary,
    moderate, generational, constrained, continental).

% Literalist critics, most prominently the Zahiri school, hold that only explicit revealed texts bind and that reliance on unrecorded communal practice is unauthorized innovation. They press the objection in writing and public disputation but are not admitted into the Medinan method's internal deliberations; their critiques circulate outside the school's councils.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, zahiri_text_only_critics, excluded,
    organized, biographical, mobile, continental).

% Academic historians of Islamic law reconstruct how the Medinan-practice doctrine crystallized, how it interacted with the rise of hadith criticism, and what its authority structure absorbed or displaced. They take no side in the methodological dispute and collect nothing from the arrangement.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, comparative_fiqh_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision procedure for deriving law from revelation when transmitted reports conflict or fail to cover a case: continuous communal practice serves as tie-breaker and gap-filler, anchoring rulings in an observable, continuously witnessed way of life instead of in competing narration chains.
% TRANSFER_FUNCTION: Moves interpretive authority and adjudicative finality toward the Medinan scholarly lineage and its school, and moves deference away from narration-graded reports and rival centers' methods; concretely, teaching posts, judgeships, and the power to settle disputed questions concentrate where the lineage teaches.
% ABSENT_VOICES: Early Iraqi traditionists and later Zahiri literalists would object that continuous practice can fossilize post-Prophetic accretion and that only graded chains of narration can authenticate; they stood outside the Medinan councils that certified 'amal, so the unanimity surrounding the doctrine was reached without them.
% DISAPPEARANCE_RATIONALE: If 'amal-as-source vanished overnight, every ruling resting on practice rather than isolated reports would need re-derivation from texts alone, the school's distinctive override mechanism would disappear, and the Medinan lineage's claim to custodial authority would collapse — the jurisprudential order of North Africa and al-Andalus would reorganize around narration-graded methods.
% FOUNDING_PROBLEM: After the Prophet's death, Muslims held conflicting reports and divergent regional practices, and no agreed procedure existed to determine which practice was authentically his. The Medinan community's continuous, unbroken way of life was proposed as the answer: a living witness older and broader than any chain of individual narrators.
% FOUNDING_PROBLEM_CORROBORATION: Al-Shafi'i's Risala and his reported disputations in Egypt attest the authentication problem was live while attacking the Medinan solution from outside the benefiting parties; Ibn Hazm's Zahiri works renew the attack centuries later; modern academic historians of Islamic law corroborate that the status remains disputed rather than settled. Corroboration of both the problem and its contested status therefore comes substantially from outside the beneficiary set.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.55 (medium): the arrangement genuinely coordinates — it resolves conflicts between transmitted reports, fills gaps the reports leave, and anchors law in an observably continuous way of life — while simultaneously concentrating interpretive authority in one lineage and subordinating rival claims to equal authenticity, which is the manifest-declared victim class. Suppression is 0.50: enforcement is structural and institutional (control of judgeships, teaching posts, and court procedure within the school's territories; refusal to admit practice-external methods to adjudicative finality) rather than severe or violent, and rival readings remain legally live across the umma, bounding the suppressive force. Theater is low (0.20): 'amal is genuinely operative in fatwa and adjudication throughout the interval, with only a modest late rise as appeals to Medinan precedent become partly formulaic in the mature school. Accessibility collapse is low (0.30): the alternative readings did not collapse — they consolidated into enduring rival schools — so understanding this constraint does not close off exits. Resistance is substantial (0.60): sustained critique from the Shafi'i hierarchical reform, from hadith specialists defending the finality of graded narration, and later from Zahiri literalists. The measurement series share one six-point grid (760–1260 CE): extraction climbs during crystallization and consolidation as school machinery matures, then plateaus; suppression_requirement is tracked because the story specifically traces enforcement build-out — from informal local consensus around a revered teacher to institutionalized school administration — rising 0.30 to 0.50 and flattening; theater creeps upward mildly. The trajectory is monotone, not cyclical: no oscillation mechanism is posited. Identity-lock dynamics: the Medinan lineage's exit is identity_locked in the institutional sense — the lineage has become its function as living witness; if the preservation frame broke publicly, its authority model would need re-grounding in narration-graded textualism, dissolving the school's distinctiveness.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the custodian seat should compute differently. From the Medinan lineage's position the arrangement is fidelity: continuity is the best available witness to revelation, and administering it is custodianship, not privilege. From the hadith specialist's position the same structure demotes a lifetime of grading work to a subordinate role precisely where it matters most — cases where a sound report conflicts with local practice. From rival jurists' positions it is regional entrenchment of one city's precedent over their methods' equal claims. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it. Suppression here is structural (institutional control of courts and teaching), not internalized — no omega for internalization is required, though the circularity omega captures the epistemic analogue.
 *
 * DIRECTIONALITY LOGIC:
 *   The Medinan lineage and Maliki jurists are declared beneficiaries: authority, teaching posts, and adjudicative finality flow to them, placing their derived directionality near the beneficiary end — amplified by identity-locked exit, which removes even voluntary exit as a damping factor. Hadith transmission specialists and rival school jurists are declared victims: the former lose arbitrating force where practice overrides graded reports (constrained exit keeps them near the full-target end), the latter suffer subordination of their claims but retain mobile exit into home regions, damping their effective extraction somewhat. Lay communities sit near symmetric: genuine benefit from stable, visible law, diffuse cost where local practice is ranked below Medinan precedent. The excluded Zahiri critics feed the absence picture rather than directionality. Scope mixes regional (lineage) to global (hadith enterprise), so scope amplification applies unevenly across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — authenticating Prophetic practice amid conflicting reports and divergent regional customs — is authored as contested, not dead: defenders hold it live whenever reports conflict; critics hold isnad criticism superseded it. Because status is contested and the disappearance verdict is world_rearranges, the mismatch consumer finds no dead-mandate-plus-persistence flag; the arrangement persists because the problem's resolution remains disputed, not because administrators cling to a corpse. The tangled_rope classification prevents mislabeling in both directions: the declared beneficiaries and coordination function stop a pure-extraction reading that would erase the real service the method performs for legal stability, while the declared victims and active-enforcement requirement stop a pure-coordination reading that would erase the asymmetric authority concentration the same structure produces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the jurisprudential_method_kernel — the Maliki instantiation. Would instantiating a sibling reading (Hanafi analogical, Shafi''i hierarchical, Hanbali textualist) change the constraint''s beneficiary/victim structure and epsilon?',
    'Generate the sibling readings as separate stories and compare computed classifications; the disagreement is located in whether unrecorded communal practice carries independent binding force alongside transmitted texts.',
    'Under the Shafi''i or Hanbali reading the Medinan lineage loses beneficiary standing and the hadith-specialist seat flips from payer toward beneficiary; epsilon redistributes across the seats accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one of four readings of a shared scripture-derived-law kernel.').

omega_variable(
    medinan_preservation_fidelity,
    'Did continuous Medinan practice in fact preserve Prophetic practice faithfully, as the reading''s foundational axiom asserts?',
    'Systematic comparison of documented Medinan ''amal against early hadith corpora and parallel non-Medinan practice, drawing on isnad-graded reports and the historiography of early Medinan legal development.',
    'Confirmed fidelity strengthens the coordination reading and lowers effective extraction; demonstrated drift severs the axiom''s empirical warrant and pushes the arrangement toward inertial maintenance of an inherited authority claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_preservation_fidelity, empirical, 'Empirical status of the preservation claim underlying the validity of Medinan practice.').

omega_variable(
    amal_binding_scope,
    'Does ''amal claim binding force only within territories where the Medinan method governs courts and teaching, or does it assert authority over the practice of all Muslim communities?',
    'Doctrinal analysis of Maliki legal theory on the territorial reach of ''amal, and of how non-Medinan communities'' practices were ranked in Maliki responsa and court practice.',
    'A jurisdictionally limited claim confines the arrangement''s costs to the school''s territories; a universal claim extends the affected class to all non-Medinan practice and raises scope-amplified extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amal_binding_scope, empirical, 'Territorial versus universal reach of the practice-based source.').

omega_variable(
    amal_warrant_circularity,
    'Is the warrant for ''amal circular — Medina certifying its own practice as the Prophet''s — such that the source-validity claim functions to entrench the lineage''s authority rather than track authentic practice?',
    'Test whether independent evidence (non-Medinan reports corroborating Medinan practice, companion-era documentation, comparative regional practice) supports the preservation claim without appeal to Medinan self-testimony.',
    'If the warrant is substantially circular, the extraction component of the measured epsilon is better read as authority-entrenchment than as the price of coordination, shifting outsider seats toward the full-target end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amal_warrant_circularity, conceptual, 'Circularity of the self-certifying warrant for Medinan practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 760, 1260).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t760, jurisprudential_method_kernel__maliki_reading, theater_ratio, 760, 0.1).
narrative_ontology:measurement_basis(juri_tr_t760, observed).
narrative_ontology:measurement(juri_tr_t860, jurisprudential_method_kernel__maliki_reading, theater_ratio, 860, 0.12).
narrative_ontology:measurement_basis(juri_tr_t860, observed).
narrative_ontology:measurement(juri_tr_t960, jurisprudential_method_kernel__maliki_reading, theater_ratio, 960, 0.15).
narrative_ontology:measurement_basis(juri_tr_t960, observed).
narrative_ontology:measurement(juri_tr_t1060, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1060, 0.18).
narrative_ontology:measurement_basis(juri_tr_t1060, observed).
narrative_ontology:measurement(juri_tr_t1160, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1160, 0.2).
narrative_ontology:measurement_basis(juri_tr_t1160, observed).
narrative_ontology:measurement(juri_tr_t1260, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1260, 0.2).
narrative_ontology:measurement_basis(juri_tr_t1260, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t760, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 760, 0.35).
narrative_ontology:measurement_basis(juri_be_t760, observed).
narrative_ontology:measurement(juri_be_t860, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 860, 0.45).
narrative_ontology:measurement_basis(juri_be_t860, observed).
narrative_ontology:measurement(juri_be_t960, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 960, 0.52).
narrative_ontology:measurement_basis(juri_be_t960, observed).
narrative_ontology:measurement(juri_be_t1060, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1060, 0.55).
narrative_ontology:measurement_basis(juri_be_t1060, observed).
narrative_ontology:measurement(juri_be_t1160, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1160, 0.56).
narrative_ontology:measurement_basis(juri_be_t1160, observed).
narrative_ontology:measurement(juri_be_t1260, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1260, 0.55).
narrative_ontology:measurement_basis(juri_be_t1260, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t760, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 760, 0.3).
narrative_ontology:measurement_basis(juri_su_t760, observed).
narrative_ontology:measurement(juri_su_t860, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 860, 0.38).
narrative_ontology:measurement_basis(juri_su_t860, observed).
narrative_ontology:measurement(juri_su_t960, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 960, 0.45).
narrative_ontology:measurement_basis(juri_su_t960, observed).
narrative_ontology:measurement(juri_su_t1060, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1060, 0.48).
narrative_ontology:measurement_basis(juri_su_t1060, observed).
narrative_ontology:measurement(juri_su_t1160, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1160, 0.5).
narrative_ontology:measurement_basis(juri_su_t1160, observed).
narrative_ontology:measurement(juri_su_t1260, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1260, 0.5).
narrative_ontology:measurement_basis(juri_su_t1260, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, information_standard).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Islamic jurisprudential method' conflates four structurally distinct claims about how law derives from Qur'an and Sunna. Per the epsilon-invariance principle, each is authored as its own story: this file is the Maliki reading (medium epsilon on custom/practice; Medinan lineage benefits; non-Medinan claims to equal authenticity pay). The Shafi'i reading is downstream in reaction — its four-tier codification was built explicitly against 'amal's independent force — so this story influences it structurally while the Hanafi and Hanbali readings coexist as parallel methodological commitments. Cross-family edges run through the shared hadith-transmission infrastructure: whatever degrades the authority of graded narration reshapes every reading's operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
