% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Progressive-Abrogation Reading of Qur'anic Gender Verses (naskh via 49:13)
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   This story authors the progressive-abrogation reading of the Qur'anic
 *   gender verses as its own structurally distinct constraint, per the
 *   ε-invariance principle: the naskh-based argument that 49:13's universal
 *   dignity principle abrogates the specific gender-differentiated rulings of
 *   4:11, 2:282, and 4:34 is a different claim from the
 *   contextual-egalitarian reading (which reinterprets rather than abrogates)
 *   and from the literal-hierarchical reading (which treats the specific
 *   verses as timeless ordinance). Each reading has its own ε, its own
 *   beneficiary/victim structure, and its own classification; they are linked
 *   here only through the shared kernel_id and network edges, never merged
 *   into one story. As authored, this reading is substantially extractive: it
 *   delegitimizes an entire scholarly tradition's authority claim, reassigns
 *   legal status to women without uniform community buy-in, and imposes high
 *   identity and career costs on scholars who hold the received tradition as
 *   constitutive of their vocation.
 *
 * KEY AGENTS:
 *   - reformist_women_scholars: agenda_setter/beneficiary (moderate/constrained) — administers the abrogation argument, gains platform, risks institutional expulsion
 *   - traditionalist_ulama: payer (institutional/identity_locked) — comprehensive delegitimization of accumulated jurisprudential authority
 *   - guardianship_dependent_women: payer (powerless/trapped) — caught between rival authority claims not of their making
 *   - egalitarian_muslim_women: beneficiary (powerless/constrained) — would gain parity if the reading is locally recognized
 *   - progressive_jurisprudence_movements: beneficiary (organized/mobile) — leverages the argument for reform advocacy across jurisdictions
 *   - comparative_hermeneutics_scholars: observer (analytical) — assesses whether the extension of naskh doctrine is methodologically defensible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.81).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.62).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.81).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.87).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive-Abrogation Reading of Qur'anic Gender Verses (naskh via 49:13)").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, 'a7ac5d3f-378d-4f9a-9682-213009b366ef').
narrative_ontology:cs_kernel_codification('a7ac5d3f-378d-4f9a-9682-213009b366ef', fixed_text).
narrative_ontology:cs_authority_grounding('a7ac5d3f-378d-4f9a-9682-213009b366ef', lineage).
narrative_ontology:cs_interpretation_layer_present('a7ac5d3f-378d-4f9a-9682-213009b366ef').
narrative_ontology:cs_reading_relation('a7ac5d3f-378d-4f9a-9682-213009b366ef', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('a7ac5d3f-378d-4f9a-9682-213009b366ef', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('a7ac5d3f-378d-4f9a-9682-213009b366ef', foundational, later_general_principle_abrogates_earlier_specific_ruling).
narrative_ontology:cs_axiom_status(later_general_principle_abrogates_earlier_specific_ruling, holdable).
narrative_ontology:cs_axiom_grounding('a7ac5d3f-378d-4f9a-9682-213009b366ef', later_general_principle_abrogates_earlier_specific_ruling, conventional).
narrative_ontology:cs_axiom('a7ac5d3f-378d-4f9a-9682-213009b366ef', secondary, gender_specific_ahkam_verses_lack_permanent_binding_force).
narrative_ontology:cs_axiom_status(gender_specific_ahkam_verses_lack_permanent_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('a7ac5d3f-378d-4f9a-9682-213009b366ef', gender_specific_ahkam_verses_lack_permanent_binding_force, instrumental).
narrative_ontology:cs_reference_frame('a7ac5d3f-378d-4f9a-9682-213009b366ef', classical_naskh_doctrinal_boundary).
narrative_ontology:cs_drift_state('a7ac5d3f-378d-4f9a-9682-213009b366ef', contemporary_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a7ac5d3f-378d-4f9a-9682-213009b366ef', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, reformist_women_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, egalitarian_muslim_women).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, progressive_jurisprudence_movements).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditionalist_ulama).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, literalist_congregants).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, guardianship_dependent_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance the naskh-based reading in academic and activist venues, arguing that 49:13's universal dignity principle chronologically and normatively supersedes earlier gender-specific verses (4:11, 2:282, 4:34). They administer the interpretive apparatus that decides which verses are abrogated and which stand, and gain professional standing and institutional platforms as the reading gains traction, but face expulsion from traditional scholarly certification bodies if they push the reading inside those institutions.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, reformist_women_scholars, agenda_setter,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, reformist_women_scholars, beneficiary).

% Would gain full legal parity in inheritance, testimony, and guardianship matters if the abrogation reading were adopted as binding jurisprudence in their communities. Currently must navigate mixed communities where the reading is contested; their exit from literalist structures depends on whether local religious authorities and family networks recognize the reading as legitimate.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, egalitarian_muslim_women, beneficiary,
    powerless, biographical, constrained, global).

% Live under literal guardianship and inheritance rules in communities that reject the abrogation reading outright. Where the progressive reading is imposed by external legal or activist pressure without local buy-in, they can be caught between rival authority claims — losing standing with their own community's religious leadership without gaining recognized standing elsewhere, and bearing the social cost of a contested reassignment of their legal status they did not choose.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, guardianship_dependent_women, payer,
    powerless, biographical, trapped, local).

% Their scholarly authority, credentialing power, and centuries of accumulated jurisprudential consensus (ijma) on gender verses are comprehensively delegitimized if abrogation displaces the specific-rule verses as binding law. Their institutional identity is constituted by transmission of the received legal tradition; adopting the abrogation reading would require repudiating their own formation and teaching lineage, which is not a reversible career or identity move.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditionalist_ulama, payer,
    institutional, civilizational, identity_locked, global).

% Structure family law, inheritance planning, and religious identity around the literal reading of the specific verses. If the abrogation reading is adopted as authoritative in their jurisdiction or community, the legal and moral framework their lives are built on is declared superseded from outside, without their having been party to the interpretive move that did it.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literalist_congregants, payer,
    powerless, generational, trapped, local).

% Organizations and reform networks that use the abrogation argument to press for legal reform in family courts and religious institutions. They gain leverage, funding, and legitimacy from the reading's adoption, and can relocate their advocacy across jurisdictions depending on where the argument gains traction, giving them exit options the local communities they advocate for and against do not have.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_jurisprudence_movements, beneficiary,
    organized, generational, mobile, global).

% Study the naskh doctrine's own internal history — which verses classical jurists already treated as abrogated, on what criteria — and can assess whether extending naskh to the gender verses is a defensible extension of an existing doctrine or a novel application invented to reach a predetermined egalitarian conclusion.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, comparative_hermeneutics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, diffuse).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinally-grounded mechanism (naskh, an established and centuries-old principle within classical Islamic jurisprudence) for resolving apparent tension between specific gender-differentiated verses and general equity verses, without requiring wholesale rejection of scriptural authority — it lets adherents remain within the tradition while reaching egalitarian legal outcomes.
% TRANSFER_FUNCTION: Moves interpretive authority and legal legitimacy away from traditionalist ulama and the accumulated consensus (ijma) built on the literal reading, toward reformist scholars and the communities and institutions that adopt the abrogation argument; moves legal standing (inheritance shares, testimony weight, guardianship exemption) from guardianship-dependent women's prior status to a claimed parity status, with the transfer's actual delivery depending on whether local authorities recognize the reading as binding.
% ABSENT_VOICES: Guardianship-dependent women living under literal readings are rarely themselves the ones advancing or contesting the abrogation argument in scholarly venues — the argument is made largely by scholars and activists on their behalf, in institutions those women do not attend. Traditionalist ulama's own internal criteria for what naskh doctrine classically covers are frequently absent from popular presentations of the progressive-abrogation argument, which tends to state the conclusion (49:13 supersedes 4:34) without engaging the classical scholarly criteria for establishing abrogation between verses.
% DISAPPEARANCE_RATIONALE: If the progressive-abrogation reading disappeared as a live interpretive option, reform litigation and reformist jurisprudential scholarship built on the naskh argument would lose their doctrinal anchor; family courts in jurisdictions that have begun citing the argument would revert to relying solely on literalist or contextualist readings; reformist scholars' professional platforms built on this specific argument would need a different doctrinal basis.
% FOUNDING_PROBLEM: Reformist scholars sought an interpretive path to gender-egalitarian legal outcomes that remains internally defensible within Islamic legal methodology (usul al-fiqh) rather than requiring rejection of scriptural authority — naskh, an already-accepted classical tool for resolving verse conflicts, was extended to gender verses to solve the problem of reaching egalitarian conclusions without abandoning claims to orthodoxy.
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars attest the trajectory-reading solves a genuine internal methodological problem. Traditionalist ulama and classical hermeneutics scholars dispute this from outside the reformist camp, arguing classical naskh doctrine was applied only to verses of legal ruling in near-contemporaneous revelation, not across the whole Qur'an's chronology to override specific ahkam verses with general ones — a methodological objection independent of whether the egalitarian outcome is desirable. No consensus corroboration exists from a neutral third party; the dispute is live within the field of usul al-fiqh itself.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at interval end) because the reading, if adopted as binding, does not merely reinterpret the specific verses — it declares them superseded, a complete normative reversal that transfers legal authority away from the tradition that has held it. Suppression is moderate (0.62): the reading itself does not typically operate through coercive enforcement machinery of its own, but its adoption in family courts or reform movements does require actively displacing the literalist reading's institutional footing, and traditionalist authorities resist with their own suppressive countermeasures (excommunication of reformist scholars, refusal of certification). Accessibility collapse is authored low (0.35) — the sibling readings remain fully live and contested; this reading has not achieved anything close to interpretive monopoly. Resistance is very high (0.87), reflecting the intensity of traditionalist scholarly and communal pushback against the abrogation claim specifically (as distinct from the milder contextualist reading, which draws less resistance because it does not claim supersession).
 *
 * PERSPECTIVAL GAP:
 *   From the reformist agenda-setter seat, this reading looks like a rope: a doctrinally sound tool (naskh) resolving genuine scriptural tension, producing net benefit for women. From the traditionalist payer seat, the same structure looks like a snare: an argument constructed to reach a predetermined conclusion, dressed in classical methodology it does not actually satisfy by classical criteria, that requires the destruction of an entire tradition's authority to succeed. The engine computes both per-seat readings from the same structural data; the tangled_rope classification here reflects that a real coordination function (resolving verse tension within an internally coherent methodology) and a real asymmetric extraction (comprehensive delegitimization of one party's authority, imposed costs on some of the very women in whose name the reading is advanced) both hold simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and progressive jurisprudence movements sit near the beneficiary end: they gain interpretive authority, platform, and reform leverage from the reading's spread. Traditionalist ulama sit near the full-target end: their authority claim is the thing being delegitimized, and their exit is identity-locked because their vocation is constituted by transmission of the tradition the reading supersedes. Guardianship-dependent women and literalist congregants are targets in a different sense — not beneficiaries of the old rule being celebrated, but payers of the disruption cost when a reassignment of legal status is imposed on their community from outside without their community's own authorities recognizing it, leaving them without stable standing under either framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two collapses: reading this purely as coordination (a benign scholarly clarification with no losers) would erase the genuine costs borne by traditionalist scholars and by women trapped between rival authority claims not of their making; reading this purely as extraction (cynical argument-construction with no genuine coordination function) would erase that naskh is an accepted classical doctrine being extended, not invented from nothing, and that many advocates hold the reading in good methodological faith. Tangled Rope holds both: genuine doctrinal coordination function AND asymmetric extraction riding through the same interpretive mechanism, requiring active advocacy and institutional pressure to displace the entrenched reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_doctrinal_extension_validity,
    'Does extending the classical naskh doctrine (historically applied to legal verses in near-contemporaneous revelation contexts) to override gender-specific ahkam verses with a distant general-principle verse (49:13) satisfy the classical criteria for establishing abrogation, or is this a novel application invented to reach a predetermined egalitarian conclusion?',
    'Close comparative analysis of classical abrogation criteria (chronological proximity, explicit contradiction rather than general/specific relationship, consensus among early jurists on which verses were abrogated) against how the progressive-abrogation argument applies naskh to 49:13 versus 4:34/4:11/2:282.',
    'If the extension satisfies classical criteria, the reading has stronger internal methodological legitimacy and the coordination function is more genuine; if it does not, the reading functions closer to a constructed cover for a reform agenda, shifting the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_doctrinal_extension_validity, conceptual, 'Whether the abrogation argument is a defensible extension of classical naskh doctrine or a novel invention.').

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading (progressive_abrogation) of the quranic_gender_verses kernel; its siblings (literal_hierarchical, contextual_egalitarian) locate the disagreement differently. Where exactly does the structural disagreement sit — is it about whether the specific verses are timeless (literal_hierarchical''s premise), about whether they require historically-situated reinterpretation without supersession (contextual_egalitarian''s premise), or about whether they are formally abrogated by a later verse (this reading''s premise)?',
    'Structural comparison of the three readings'' core premises against the same source verses — the disagreement is located in the mechanism of change (no change vs. contextual reinterpretation vs. formal abrogation), not in the underlying ethical commitment to some notion of dignity, which most readings share at some level.',
    'If the disagreement is genuinely about mechanism rather than outcome, then progressive_abrogation and contextual_egalitarian may converge in practical legal outcome while diverging sharply on the traditionalist ulama''s ability to accommodate them — abrogation forecloses the ulama''s authority claim in a way reinterpretation does not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Where the three sibling readings of the kernel actually locate their structural disagreement.').

omega_variable(
    epistemic_violence_vs_liberation_framing,
    'For communities whose religious and legal identity is substantially constituted by the literal reading of the specific gender verses, does the imposition of the abrogation reading (via external legal reform, activism, or academic pressure) constitute epistemic violence against their tradition, or does it constitute liberation of women whose interests the tradition has historically subordinated to communal continuity?',
    'Longitudinal study of communities where the abrogation reading has been adopted by external legal mandate versus internal scholarly consensus, tracking whether women within those communities report increased or decreased standing, and whether traditionalist authority structures adapt, fragment, or entrench in response.',
    'This is a genuinely preference-laden question — the answer depends on how one weighs communal self-determination in religious interpretation against individual women''s legal parity, and different observers will weigh it differently even given full empirical information.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_violence_vs_liberation_framing, preference, 'Whether imposing the abrogation reading is epistemic violence, liberation, or both depending on the seat.').

omega_variable(
    beneficiary_uniformity_ambiguity,
    'Is ''egalitarian_muslim_women'' a uniform beneficiary class, or does the reading actually benefit a subset (women in cosmopolitan, legally pluralistic contexts who can invoke the reading) while leaving guardianship_dependent_women in more literalist enclaves structurally worse off (caught between rival claims, recognized by neither authority as having stable status)?',
    'Disaggregated survey of legal outcomes for women across jurisdictions/communities where the abrogation reading has and has not achieved institutional recognition, controlling for baseline guardianship regime strength.',
    'If the beneficiary class fractures this way, the tangled_rope classification understates internal victim heterogeneity — some declared beneficiaries may in practice sit closer to the payer end depending on their local institutional context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_uniformity_ambiguity, empirical, 'Whether the declared beneficiary group is uniform or internally divided by local institutional recognition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__progressive_abrogation, theater_ratio, 8, 0.16).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__progressive_abrogation, theater_ratio, 16, 0.19).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__progressive_abrogation, theater_ratio, 24, 0.22).
narrative_ontology:measurement(qura_tr_t32, quranic_gender_verses__progressive_abrogation, theater_ratio, 32, 0.25).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__progressive_abrogation, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__progressive_abrogation, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__progressive_abrogation, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(qura_be_t32, quranic_gender_verses__progressive_abrogation, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__progressive_abrogation, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__progressive_abrogation, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__progressive_abrogation, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(qura_su_t32, quranic_gender_verses__progressive_abrogation, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__progressive_abrogation, 0.08).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the quranic_gender_verses kernel, each authored as a separate ε-invariant story: literal_hierarchical (the specific verses are timeless divine ordinance, unabrogated), contextual_egalitarian (the verses are historically situated progressive steps requiring reinterpretation under maqasid without claiming formal abrogation), and this one, progressive_abrogation (the specific verses are formally superseded by 49:13 via naskh). The three share source verses and a contested kernel but have different beneficiary/victim structures, different ε, and different classifications — literal_hierarchical treats the current arrangement as settled and low-extraction from its own seat; contextual_egalitarian claims a milder reinterpretive extractiveness without full supersession; this reading claims the highest extractiveness because it asserts complete normative reversal and comprehensive delegitimization of the literalist authority structure. They are linked via affects_constraints rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
