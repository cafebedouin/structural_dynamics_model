% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Isaac Covenant Exclusivity (Reading: Transmitted exclusively to Isaac's line; Ishmael excluded)
 *   domain: religious/institutional/identity
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested Abrahamic
 *   covenant kernel: the Isaac-exclusive reading, in which Genesis 17:19-21
 *   is interpreted as limiting covenant transmission to Isaac's descendants,
 *   explicitly excluding Ishmael. This reading has served as the
 *   authoritative institutional frame for Jewish covenant identity for
 *   approximately 2,000 years across diverse Jewish communities. The
 *   constraint simultaneously coordinates Jewish intergenerational identity
 *   (genuine coordination function) and extracts status and legitimacy from
 *   Ishmaelite claimants and Islamic tradition (asymmetric extraction). This
 *   is a kernel reading: the same Genesis text is interpreted differently by
 *   sibling readings (ishmael_covenant_reading, which includes Ishmael; and
 *   the land_promise_constraint reading, which disputes the territorial
 *   dimension). The claim/metric divergence is intentional and structural:
 *   the reading is CLAIMED as tangled_rope (mixed coordination and
 *   extraction) while the metrics describe substantially extractive, actively
 *   enforced institutional operation. The engine computes per-seat types from
 *   the structural data; the commentary explains why payer and beneficiary
 *   seats experience different constraint types.
 *
 * KEY AGENTS:
 *   - jewish_institutional_authority: Agenda-setter, interprets and enforces the reading, benefits from covenant exclusivity
 *   - isaac_line_membership: Beneficiaries (religious identity, covenant standing), identity-locked participants
 *   - ishmaelite_claimants: Victims (excluded from covenant status and institutional authority), organized responders
 *   - islamic_tradition_covenant_claims: Victims (structural denial of covenant legitimacy), institutional competitor
 *   - textual_interpreters: Observers (analysts of the reading's textual basis and defensibility)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.72).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.68).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Isaac Covenant Exclusivity (Reading: Transmitted exclusively to Isaac's line; Ishmael excluded)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/institutional/identity").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '83b01293-8396-453a-8769-c617e8c2e0f2').
narrative_ontology:cs_kernel_codification('83b01293-8396-453a-8769-c617e8c2e0f2', fixed_text).
narrative_ontology:cs_authority_grounding('83b01293-8396-453a-8769-c617e8c2e0f2', lineage).
narrative_ontology:cs_interpretation_layer_present('83b01293-8396-453a-8769-c617e8c2e0f2').
narrative_ontology:cs_reading_relation('83b01293-8396-453a-8769-c617e8c2e0f2', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('83b01293-8396-453a-8769-c617e8c2e0f2', abrahamic_covenant__land_promise_constraint, influences).
narrative_ontology:cs_axiom('83b01293-8396-453a-8769-c617e8c2e0f2', foundational, covenant_transmission_exclusively_isaac).
narrative_ontology:cs_axiom_status(covenant_transmission_exclusively_isaac, holdable).
narrative_ontology:cs_axiom_grounding('83b01293-8396-453a-8769-c617e8c2e0f2', covenant_transmission_exclusively_isaac, deontological).
narrative_ontology:cs_axiom('83b01293-8396-453a-8769-c617e8c2e0f2', secondary, ishmael_explicitly_excluded_from_covenant_standing).
narrative_ontology:cs_axiom_status(ishmael_explicitly_excluded_from_covenant_standing, holdable).
narrative_ontology:cs_axiom_grounding('83b01293-8396-453a-8769-c617e8c2e0f2', ishmael_explicitly_excluded_from_covenant_standing, deontological).
narrative_ontology:cs_reference_frame('83b01293-8396-453a-8769-c617e8c2e0f2', genesis_17_19_21_isaac_exclusive_covenant).
narrative_ontology:cs_drift_state('83b01293-8396-453a-8769-c617e8c2e0f2', contemporary_interfaith_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('83b01293-8396-453a-8769-c617e8c2e0f2', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_continuity).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, isaac_line_membership).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_tradition_covenant_claims).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, divine_election_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, chosenness_through_isaac).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits the Isaac-exclusive reading of Genesis 17:19-21 as the authoritative covenant frame. Teaches this reading to subsequent generations through rabbinic interpretation, halakhic development, and liturgical practice. The reading stabilizes Jewish identity continuity and validates institutional authority to mediate covenant relationship. Can shift the reading through new textual interpretation, but such shifts would require internal theological consensus and face accumulated institutional investment in the exclusive reading.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_authority, agenda_setter,
    institutional, civilizational, mobile, global).

% Receives the status of being the exclusive heir to the Abrahamic covenant. This identity confers both privilege (covenant membership, chosenness, election) and obligation (law, commandment, ritual practice). Members experience covenant membership as constitutive of their identity; the exclusive-Isaac reading grounds their claim to continuity and divine relationship. Exit would require abandoning Jewish identity itself, not merely disagreeing with institutional authority.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, isaac_line_membership, beneficiary,
    organized, civilizational, identity_locked, global).

% Islamic and historical Ishmaelite tradition claims Abraham as patriarch and founder, yet the Isaac-exclusive reading denies Ishmael covenant standing. Ishmaelite claimants bear the cost of exclusion from Jewish institutional covenant claims and face the constraint's delegitimation of their own covenant genealogy. They can develop competing readings (ishmael_covenant_reading) but cannot compel institutional Jewish authority to adopt them. Their exit options are constrained to counterclaim, reframe, or organize separate covenant theology.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants, payer,
    organized, civilizational, constrained, global).

% Islamic tradition explicitly claims Abraham as forefather (Ibrahim), develops covenant theology that includes Ishmael as progenitor of the Arab peoples and founder of the Kaaba, and asserts Muhammad as the final prophet continuing the Abrahamic prophetic line. The Isaac-exclusive reading structurally denies Islamic covenant claims standing within the Abrahamic framework and creates institutional pressure against acknowledging Islamic theology as legitimate heir to Abraham. Islamic tradition responds by developing its own covenant reading (ishmael_covenant_reading), but institutional Jewish authority has no structural incentive to validate it.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_tradition_covenant_claims, payer,
    organized, civilizational, constrained, global).

% Supersessionist Christianity claims the covenant is fulfilled in and transferred to the Church, making the Isaac-exclusive reading partly redundant and partly contradictory to Christian replacement theology. Christian authority is excluded from shaping the Isaac-exclusive reading's internal development and cannot directly contest it within Jewish institutional discourse, yet the two readings occupy overlapping theological space and compete for interpretive authority over Abraham's covenant legacy.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_authority, excluded,
    institutional, civilizational, trapped, global).

% Scholars and analysts across Jewish, Islamic, and secular academic traditions study the textual basis of competing covenant readings. They examine Genesis 17:19-21 lexically, historically, and narratively to assess the textual warrant for exclusive vs. inclusive interpretations. They can produce evidence that constrains what readings are defensible, but do not adjudicate the reading's adoption by institutional authority.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, textual_interpreters, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_authority).
narrative_ontology:fixing_cost_class(abrahamic_covenant__isaac_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a durable religious lineage and institutional continuity framework: defines who is a member of the covenant community across generations, who has standing to interpret the covenant, and what obligations and privileges attach to membership. Solves the problem of intergenerational identity persistence and theological authority transmission.
% TRANSFER_FUNCTION: Transfers covenant status (membership, chosenness, election, legal obligation, spiritual relationship to the divine) from Abraham exclusively through Isaac's descendants, withholding that status from Ishmael's descendants and later claimants. Also transfers institutional interpretive authority to Jewish institutional structures that control the reading's transmission and application.
% ABSENT_VOICES: Ishmael himself (a historical figure whose own claims cannot be heard in the text), modern Ishmaelite descendants and Islamic tradition (excluded from shaping the reading within Jewish institutional discourse), alternative Jewish readings that deny exclusive Isaac transmission (marginalized within normative Jewish institutional authority), and secular scholars who question the reading's historical or literary basis (not admitted as adjudicators, only as observers).
% DISAPPEARANCE_RATIONALE: Jewish institutional identity would face existential reorientation — the exclusive-Isaac reading is constitutive of normalized Jewish identity and intergenerational continuity. Islamic tradition would experience it as vindication of their counterclaim to Abrahamic covenant standing. Christian supersessionist authority would lose a competitor reading against which to define its own position. The reading's disappearance would require institutional adoption of an inclusive reading (ishmael_covenant_reading) or abandonment of covenant identity altogether; some view this as liberation, others as catastrophic institutional dissolution.
% FOUNDING_PROBLEM: After Abraham's death, how does his covenant relationship to the divine continue? Which of his descendants carry covenant status and authority? The Genesis narrative faces an interpretive choice at Genesis 17:19-21: God explicitly states the covenant continues through Isaac (not Ishmael). The founding problem is the succession question—ensuring coherence and legitimacy across generations.
% FOUNDING_PROBLEM_CORROBORATION: Jewish institutional and rabbinic tradition attests that the succession problem is solved by the Isaac-exclusive reading and remains live (covenant continuity is ongoing obligation and privilege). Islamic tradition and Ishmaelite descendants attest the founding problem is misread—Abraham's covenant extends to all his descendants including Ishmael. Textual scholars (outside the benefiting parties) note the Genesis 17:19-21 passage is ambiguous: it explicitly names Isaac but does not use the word 'only' (exclusive language is interpretive, not textual). No corroborating source from outside the benefiting parties attests that the exclusive-Isaac reading is the unique defensible reading.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, contested).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the reading asymmetrically allocates covenant status to Isaac's descendants while withholding it from Ishmael's, and this allocation is sustained by institutional authority rather than by participant preference or logical necessity. The textual basis (Genesis 17:19-21) does not explicitly use exclusive language ('only' does not appear in the Hebrew); the exclusive reading is interpretive, and competing interpretations are textually defensible. Suppression is substantial (0.68) because the exclusive reading is maintained by institutional control of interpretation, restricted access to authoritative textual commentary, and social pressure within Jewish communities to accept the normalized reading. Theater is moderate (0.41): the reading has genuine coordination function (provides durable identity framework), but enforcement increasingly depends on ritual practice and institutional authority rather than on compelling theological argument (as competitors emerge). The measurement series show slight extraction drift upward from 0.68 to 0.72 over 2000 years, correlating with crystallization of the reading into institutional practice and increasing Islamic and secular challenge to the reading's warrant. Theater ratio rises modestly as theological justification becomes more ritualized and less empirically grounded. Suppression requirement rises as the reading faces organized institutional challenge from Islamic and secular perspectives. The constraint appears to reach a plateau around t=1400 (medieval period) as the reading becomes institutionally locked-in across Jewish communities.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (ishmaelite and islamic tradition) experience the constraint as a snare: institutional denial of their covenant claims with no offsetting coordination benefit—they are pure targets of the extraction (lost status and legitimacy). The agenda-setter seat (jewish institutional authority) experiences it as rope: genuine coordination (intergenerational identity stabilization) that they administer and benefit from. The beneficiary seat (isaac_line_membership) sits between: coordination benefit (identity, belonging) but also extraction (obligation without consent, identity lock). The engine's per-seat computation should capture this divergence: the constraint looks like Rope from the beneficiary/agenda-setter perspective and Snare from the payer perspective. This divergence is not a classification error—it is the structural asymmetry the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   For jewish_institutional_authority (agenda_setter, institutional power): d ≈ 0.15 (full beneficiary). The reading benefits institutional authority directly: it validates their interpretive legitimacy, sustains their role as covenant mediators, and concentrates interpretive power in their hands. They face no suppression or extraction; they enforce the reading. Their exit option is mobile—they could reinterpret, but institutional investment makes reinterpretation costly. From their seat, the constraint computes as Rope (coordination with beneficiary advantage). For isaac_line_membership (beneficiary, organized, identity_locked): d ≈ 0.35 (near-beneficiary). Members receive covenant status and religious identity that is constitutive of their self-concept. Their exit is identity_locked: leaving would require abandoning Judaism. The extraction cost is asymmetric—they bear theological obligation ('chosen for commandment') that non-members avoid. From their seat, the constraint computes as complex: coordination benefit (durable identity) plus extraction (obligation without choice). For ishmaelite_claimants and islamic_tradition (payer, organized, constrained exit): d ≈ 0.85 (near-target). They bear the cost of exclusion: delegitimation of their covenant claim, institutional pressure against their alternative reading, structural denial of standing in Jewish institutional discourse. Their exit options are constrained—they can counterclaim and develop competing readings (ishmael_covenant_reading), but cannot compel institutional Jewish authority to validate them. From their seat, the constraint computes as Snare: pure extraction of legitimacy with no coordination benefit, sustained by institutional coercion (control of interpretation) and limited exit (constrained to reframing rather than escape).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (founding mandate has outlived its function) is present but contested. The founding problem is succession after Abraham: ensuring covenant continuity across generations. For jewish institutional authority and isaac_line_membership, the founding problem is LIVE: covenant continuity is still the active mandate (ongoing obligation and privilege). For ishmaelite claimants and islamic tradition, the founding problem is DEAD or INVERTED: the exclusive-Isaac reading was never their mandate; it was imposed as an exclusion. The constraint persists despite founding-problem divergence because jewish institutional authority has the structural power to maintain it (institutional control, social conformity pressure, identity lock). This is the signature of a constraint whose mandate has been reinterpreted rather than abandoned—the original succession problem has evolved into an identity-boundary-maintenance problem, which serves different interests (institutional stability vs. exclusionary status). The tangled_rope claim (mixed coordination and extraction) captures this mandatrophy condition: the constraint genuinely solves a coordination problem (identity continuity) AND serves extractive interests (exclusion of competitors). If the founding mandate (succession after Abraham) were the only operative concern, the constraint would decompose into rope (pure coordination). That it doesn't reflects that the constraint has accumulated purposes beyond the original founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_warrant_for_exclusivity,
    'Does Genesis 17:19-21 textually mandate exclusive transmission through Isaac, or is the exclusivity an interpretive choice?',
    'Lexical analysis of the Hebrew text; comparison with other biblical passages on covenant transmission (e.g., Genesis 21:13 regarding Ishmael); study of pre-rabbinic interpretive traditions (Dead Sea Scrolls, Septuagint, Targumim) to assess the range of defensible readings.',
    'If the text does not mandate exclusivity, the reading is interpretive and contestable, and the extraction component becomes more visible—the constraint is sustaining a particular reading via institutional authority rather than enforcing a natural law. This would support reclassification toward Snare for payer seats and Tangled Rope for beneficiary seats. If the text does mandate exclusivity (through linguistic or contextual necessity), the exclusive reading is more defensible as coordination rather than pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_warrant_for_exclusivity, empirical, 'Whether the exclusive-Isaac reading has sufficient textual warrant or is fundamentally interpretive').

omega_variable(
    identity_lock_mechanism_ambiguity,
    'Is the identity_locked exit option for isaac_line_membership structural (persons cannot leave without ceasing to be Jewish) or internalized (persons believe they cannot leave, but exit is actually available)?',
    'Post-exit analysis: if persons who leave Judaism experience persistent identity attachment or social reintegration into Jewish community despite formal exit, the identity lock is partly structural; if exit is clean and identity attachment dissolves, the identity lock is partly internalized.',
    'If identity lock is structural, the constraint''s extraction of obligation is genuine and unavoidable—beneficiaries truly cannot exit. If internalized, the extraction is higher than the structural measure suggests—the constraint has embedded itself in beneficiaries'' self-concept and they carry the lock with them even if they leave. This affects the theater_ratio assessment: higher internalization means more of the extraction is theatrical (self-maintained rather than institutionally enforced).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_ambiguity, empirical, 'Whether the identity_locked exit option reflects structural barriers or internalized constraints').

omega_variable(
    institutional_authority_veto_power,
    'Can jewish institutional authority be overridden or reinterpreted by competing internal Jewish voices (heterodox readings, secular Jews, interfaith interpreters), or does institutional authority function as an effective veto?',
    'Documentary analysis of Jewish institutional discourse over the past 200 years: tracking whether competing readings (e.g., historical-critical scholarship, reformist theology) have gained standing within Jewish institutional spaces, and whether institutional authority has responded by reinterpreting or maintained the exclusive-Isaac reading despite scholarly and theological pressure.',
    'If institutional authority functions as a veto (competing readings are marginalized despite internal Jewish support), the suppression score is accurate and the constraint is actively enforced. If competing readings have gained standing and institutions have begun to accommodate plural readings, suppression should be scored lower and the constraint''s enforcement is weaker than measured. This affects the per-seat classification for beneficiaries: if institutional authority is absolute, beneficiaries experience the constraint as coordination they trust; if institutional authority is contested, beneficiaries may experience it as enforcement they resent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_veto_power, empirical, 'Whether institutional Jewish authority functions as an effective veto on competing covenant readings').

omega_variable(
    sibling_reading_validity_contest,
    'Which of the sibling readings (ishmael_covenant_reading, land_promise_constraint) has greater textual, theological, and institutional warrant—and does this contest resolve or amplify the mandatrophy condition?',
    'Comparative analysis of the three readings'' textual bases, institutional adoption across different faith traditions, and logical coherence with the broader Abrahamic narrative. Assessment of whether one reading is demonstrably more defensible or whether the contest is fundamentally unresolvable within the confines of the inherited text.',
    'If ishmael_covenant_reading proves more textually defensible, the isaac_covenant_reading becomes vulnerable to institutional reinterpretation or abandonment—mandatrophy would shift toward resolution (constraint dissolves or reframes). If both readings remain textually defensible and institutionally entrenched, the mandatrophy condition hardens: the constraint persists despite contested warrant, pure institutional power. This is the central uncertainty for kernel readings—the contest between siblings determines whether mandatrophy indicates a temporary institutional inertia or a permanent structural deadlock.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_validity_contest, conceptual, 'Whether the sibling readings'' validity contest resolves the kernel''s interpretive ambiguity or locks mandatrophy into place').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(abra_tr_t0, observed).
narrative_ontology:measurement(abra_tr_t300, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 300, 0.38).
narrative_ontology:measurement_basis(abra_tr_t300, observed).
narrative_ontology:measurement(abra_tr_t800, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 800, 0.4).
narrative_ontology:measurement_basis(abra_tr_t800, observed).
narrative_ontology:measurement(abra_tr_t1400, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1400, 0.41).
narrative_ontology:measurement_basis(abra_tr_t1400, observed).
narrative_ontology:measurement(abra_tr_t1700, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1700, 0.41).
narrative_ontology:measurement_basis(abra_tr_t1700, observed).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(abra_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(abra_be_t0, observed).
narrative_ontology:measurement(abra_be_t300, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 300, 0.7).
narrative_ontology:measurement_basis(abra_be_t300, observed).
narrative_ontology:measurement(abra_be_t800, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 800, 0.71).
narrative_ontology:measurement_basis(abra_be_t800, observed).
narrative_ontology:measurement(abra_be_t1400, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1400, 0.72).
narrative_ontology:measurement_basis(abra_be_t1400, observed).
narrative_ontology:measurement(abra_be_t1700, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1700, 0.72).
narrative_ontology:measurement_basis(abra_be_t1700, observed).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement_basis(abra_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(abra_su_t0, observed).
narrative_ontology:measurement(abra_su_t300, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 300, 0.64).
narrative_ontology:measurement_basis(abra_su_t300, observed).
narrative_ontology:measurement(abra_su_t800, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 800, 0.67).
narrative_ontology:measurement_basis(abra_su_t800, observed).
narrative_ontology:measurement(abra_su_t1400, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1400, 0.68).
narrative_ontology:measurement_basis(abra_su_t1400, observed).
narrative_ontology:measurement(abra_su_t1700, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1700, 0.68).
narrative_ontology:measurement_basis(abra_su_t1700, observed).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(abra_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__isaac_covenant_reading, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__land_promise_constraint).

% DUAL FORMULATION NOTE:
% This constraint is part of the abrahamic_covenant constraint family. The family decomposes the contested transmission of Abraham's covenant relationship into three structurally distinct claims: (1) isaac_covenant_reading—covenant exclusive to Isaac's line (this story); (2) ishmael_covenant_reading—covenant inclusive of Ishmael, continuing to Islamic tradition; (3) land_promise_constraint—territorial dimension (Land of Canaan) as covenant component. Each reading has a different ε (different interpretation = different extraction profile), different beneficiary/victim structure, and different institutional status. They are linked as a family because each reading's validity affects the others' institutional standing and theological defensibility. The constraint family exhibits the ε-invariance principle: the same kernel (Abraham's covenant) cannot be analyzed as one constraint with multiple observables; it requires decomposition into sibling readings, each with its own ε, each with its own stakeholders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__isaac_covenant_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
