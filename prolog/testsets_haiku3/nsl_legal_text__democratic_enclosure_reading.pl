% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: NSL as Permanent Democratic Enclosure and Dissent Criminalization
 *   domain: constitutional_law/political_sociology
 *
 * SUMMARY:
 *   The National Security Law (NSL), enacted by Beijing and implemented by
 *   Hong Kong in 2020, criminalizes secession, subversion, terrorism, and
 *   foreign collusion with unprecedented vagueness. This reading instantiates
 *   NSL as a mechanism for the permanent closure of democratic space: the
 *   text provides cover for the criminalization of all opposition,
 *   independent journalism, civil society organizing, and academic dissent.
 *   The founding political disagreement over Hong Kong's electoral system and
 *   autonomy—which drove 2019 protests—is reframed as a security crisis,
 *   solved through legal structure that removes the possibility of democratic
 *   resolution. The constraint extracts political authority from Hong Kong's
 *   constitutional institutions and concentrates it in Beijing's national
 *   security apparatus, with Hong Kong's establishment aligned to the
 *   extraction as it consolidates their institutional monopoly. This is a
 *   kernel reading: the same legal text is read differently by the
 *   sovereignty_restoration reading (legitimate security instrument) and
 *   jurisdictional_capture reading (common-law destruction). The
 *   democratic_enclosure reading is presented here as the structural analysis
 *   of NSL's actual operation, not as a moral judgment of Beijing's right to
 *   act.
 *
 * KEY AGENTS:
 *   - Beijing central authority / CCP party-state: agenda-setter, defines NSL scope and interpretation, collects political control
 *   - Hong Kong establishment (pro-Beijing parties, judges, police command, civil service): beneficiary and secondary payer, consolidates institutional monopoly but must administer expanding prosecutions
 *   - Civil society organizations: powerless-to-organized payer, face prosecution for lawful speech and organizing
 *   - Independent press: moderate-organized payer, press outlets shuttered, editors arrested, self-censorship enforced
 *   - Opposition parties: moderate-organized payer, electoral participation criminalized, candidates disqualified
 *   - Protest participants: powerless payer, prosecution for peaceful assembly, chilling effects across entire population
 *   - Academic community (pro-autonomy scholars): moderate organized payer, publication and employment at risk for scholarly analysis
 *   - Independent legal profession: organized payer and excluded, attorney-client privilege eroded, defense itself framed as subversion
 *   - International human rights monitoring: institutional observer, external accountability preserved while excluded from Hong Kong discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.89).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.91).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "NSL as Permanent Democratic Enclosure and Dissent Criminalization").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '3a09ae90-e94f-4f1d-99ba-59e11246adcf').
narrative_ontology:cs_kernel_codification('3a09ae90-e94f-4f1d-99ba-59e11246adcf', formalized).
narrative_ontology:cs_authority_grounding('3a09ae90-e94f-4f1d-99ba-59e11246adcf', extraction).
narrative_ontology:cs_interpretation_layer_present('3a09ae90-e94f-4f1d-99ba-59e11246adcf').
narrative_ontology:cs_reading_relation('3a09ae90-e94f-4f1d-99ba-59e11246adcf', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a09ae90-e94f-4f1d-99ba-59e11246adcf', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('3a09ae90-e94f-4f1d-99ba-59e11246adcf', foundational, electoral_opposition_incompatible_with_national_security).
narrative_ontology:cs_axiom_status(electoral_opposition_incompatible_with_national_security, holdable).
narrative_ontology:cs_axiom_grounding('3a09ae90-e94f-4f1d-99ba-59e11246adcf', electoral_opposition_incompatible_with_national_security, empirically_contingent).
narrative_ontology:cs_axiom('3a09ae90-e94f-4f1d-99ba-59e11246adcf', foundational, democratic_space_restoration_impossible_through_legal_reform).
narrative_ontology:cs_axiom_status(democratic_space_restoration_impossible_through_legal_reform, holdable).
narrative_ontology:cs_axiom_grounding('3a09ae90-e94f-4f1d-99ba-59e11246adcf', democratic_space_restoration_impossible_through_legal_reform, deontological).
narrative_ontology:cs_reference_frame('3a09ae90-e94f-4f1d-99ba-59e11246adcf', hong_kong_basic_law_constitutional_autonomy).
narrative_ontology:cs_drift_state('3a09ae90-e94f-4f1d-99ba-59e11246adcf', post_nsl_implementation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('3a09ae90-e94f-4f1d-99ba-59e11246adcf', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment_aligned).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, opposition_political_parties).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, protest_movement_participants).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_autonomy_academics).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_legal_profession).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment_aligned).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dictates NSL definition, enforcement standards, and legal interpretation through national security apparatus and appointed judges. Uses the text's vague definitional boundaries (sedition, subversion, foreign collusion) to expand criminalization into any public challenge to CCP authority or Hong Kong's electoral system. Controls the machinery of prosecution and judicial review with no genuine appellate oversight. Collects political authority and institutional control from the constraint's operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Institutional actors (pro-Beijing political parties, appointed judges, police command, civil service leadership) benefit from NSL's enforcement: it removes electoral and policy competition, cements their monopoly on legitimacy narratives, eliminates threats to their institutional position. They also incur costs (having to administer expanding prosecutions, managing international criticism) but these are asymmetric with their control position. Their exit option is identity-locked: to deny the NSL's legitimacy would require disavowing the entire Beijing-aligned institutional identity they have constructed.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment_aligned, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment_aligned, payer).

% Face criminal liability for advocacy, organizing, or coordination that might be read as subversion, foreign collusion, or sedition. NGOs working on labor rights, environmental protection, or social welfare operate under existential uncertainty—the same activities that were lawful under the Basic Law (freedom of association, peaceful assembly) are now prosecutable. Their options are: self-censor to near-silence, reorganize in exile (losing on-ground constituency), or accept prosecution risk. Trapped exit stems from lack of alternative organizational venues and the cost of losing ground-level presence.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    organized, biographical, trapped, national).

% Operate under indefinite suppression: publishing investigations into police conduct, corruption, or CCP authority can be framed as sedition or foreign collusion. Shuttering of outlets (Apple Daily), arrests of editors, freezing of assets are now routine enforcement. Journalists self-censor or relocate (constrained exit, not trapped, because some can reach foreign press). Those who remain carry prosecution risk daily; those who leave lose the ability to report Hong Kong's internal news.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press, payer,
    moderate, biographical, constrained, national).

% Criminalized as a class: running candidates on pro-autonomy or pro-democracy platforms becomes sedition; fundraising from overseas supporters becomes foreign collusion; advocating for electoral reform becomes subversion. The option space has collapsed from competitive democracy to performative participation in fixed elections or exit to exile. Trapped because electoral politics is the structural venue for opposition in a would-be constitutional framework; losing it means abandoning institutional legitimacy entirely.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, opposition_political_parties, payer,
    moderate, biographical, trapped, national).

% Face prosecution for participation in protests, carrying protest materials, or online organizing. The definition of sedition and subversion captures peaceful assembly and political speech that were constitutionally protected before NSL. Prosecution has been selective, creating chilling effects beyond those arrested. Exit option is trapped: staying means continuing to live under prosecution risk; leaving means exile. The powerless structural position (no institutional leverage, no resources for legal defense) amplifies extraction.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, protest_movement_participants, payer,
    powerless, immediate, trapped, local).

% Face prosecution for academic publications, public lectures, or university governance positions. Scholars of Hong Kong history, constitutional law, or political economy who publish analyses Beijing deems threatening face investigation, employment termination, or arrest. Some retain constrained exit (moving to universities abroad, publishing with foreign presses); others remain and self-censor or accept risk. The suppression affects the entire epistemic infrastructure for understanding the constraint itself.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_autonomy_academics, payer,
    moderate, biographical, constrained, regional).

% Barristers and solicitors representing NSL defendants face professional consequences, client intimidation, and erosion of attorney-client privilege in national security cases. The legal profession's independence (a cornerstone of common law) is undermined from within—lawyers cannot fulfill their professional duty to defend clients when the political authority interprets defense itself as subversion. Many have emigrated (constrained exit); those remaining operate under institutional pressure. Excluded from genuine voice because national security prosecutions are framed outside ordinary legal process.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_legal_profession, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, independent_legal_profession, excluded).

% UN bodies, Human Rights Watch, Amnesty International, and other monitors document the constraint's operation. Their analysis is excluded from Hong Kong policy discourse (dismissed as foreign interference) but provides external accountability for the reading. Their seat reveals the transparency asymmetry: full documentation of suppression exists outside Hong Kong; none is produced within official channels.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_human_rights_monitoring, observer,
    institutional, generational, analytical, global).

% The upstream authority dictating the constraint's interpretation and enforcement. Not named as a separate stakeholder from Beijing central authority for governance clarity, but bears noting: the NSL is a structural embedding of CCP party-state authority into Hong Kong law, bypassing the Basic Law's institutional separation. This is the mechanism through which 'Beijing' operates institutionally in the constraint.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_ccp_party_state, agenda_setter,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NSL purports to coordinate against security threats (foreign interference, violent subversion, separatism) within Hong Kong. Under this reading, the stated coordination function is a cover story: the actual function is to remove institutional constraints on executive authority and criminalize democratic opposition.
% TRANSFER_FUNCTION: Transfers political authority, control over electoral outcomes, and the power to define legitimate speech from Hong Kong's constitutional institutions (legislature, judiciary, civil society) to Beijing's national security apparatus. Transfers freedom of movement, association, and expression from the population to the state. Transfers careers, safety, and liberty from opposition figures to the state's prosecutorial discretion.
% ABSENT_VOICES: Pro-democracy parties, independent civil society organizations, and international observers are structurally excluded from the policy conversation—their exclusion is one of the constraint's functions. They would argue that NSL criminalizes constitutionally protected speech and dismantles the institutional separation the Basic Law promised. Their absence from 'consensus' reflects the suppression itself, not the legitimacy of the constraint.
% DISAPPEARANCE_RATIONALE: If NSL enforcement ceased tomorrow, Hong Kong's political landscape would reorganize immediately: opposition parties could campaign openly, civil society could organize, independent press could publish investigations, students could protest without arrest. The entire architectural suppression—the chilling effects, the self-censorship, the selective prosecutions, the career risks—depends on NSL's active operation. Removing it would restore the democratic space the constraint was designed to close.
% FOUNDING_PROBLEM: Beijing frames the founding problem as: 2019 was a social unrest crisis; foreign powers (US, UK) supported protesters and pro-democracy organizations; Hong Kong's existing laws were insufficient to prevent separatism and foreign interference. NSL was needed to restore constitutional order and security.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and Hong Kong establishment authorities attest the founding problem is live and ongoing (foreign interference persists, separatism is dormant but permanent threat). Pro-democracy parties, international monitors, and academics not aligned with Beijing attest that the 2019 unrest was a genuine political disagreement over electoral reform and autonomy — not a security crisis requiring the criminalization of dissent — and that NSL's scope far exceeds any plausible security response. Legislative testimony, civil society reports, and UN monitoring bodies corroborate the second reading. The 'founding problem' is most accurately described as a political conflict over Hong Kong's constitutional trajectory, which NSL resolves through suppression rather than resolution.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.89 (near-maximal) because NSL's operation is the transfer of political authority, democratic voice, and legal protection from the entire civil society/opposition sector to a single centralized executive authority. The margin between stated security coordination (0.15–0.20) and actual operation (0.89) is massive—this is the core structural fact of the snare classification. Suppression is measured at 0.91 because the constraint's persistence depends entirely on active enforcement: prosecution machinery, selective enforcement (chilling effects without convictions), institutional coercion of judges and lawyers, and international pressure suppression. Without continuous enforcement, NSL becomes an unenforced legal text. Theater ratio at 0.62 reflects that a significant share of enforcement activity is devoted to maintaining the appearance of specific charges (sedition, subversion, foreign collusion) when the actual function is broad political suppression. The legitimacy performance has become more theatrical over time as the scope of prosecuted conduct expands well beyond plausible security responses. Accessibility collapse at 0.88 reflects that the democratic space has collapsed nearly completely for opposition: no safe electoral path, no safe organizing, no safe journalism, no safe scholarship. Resistance at 0.71 is substantially high because the constraint meets real and organized resistance: emigration movements, international pressure, civil society continuing despite risk, and internal institutional pressure from some judges. This resistance has not reversed the constraint but has stabilized it rather than allowing further ratcheting, creating a plateau. The coercion_grid shows: structural suppression reaching 0.91 (institutional enforcement fully mobilized); organizational resistance rising to 0.68 (NGOs and parties adapting and coordinating despite suppression); class-level resistance at 0.72 (broad-based civil society refusing complete silence); individual-level suppression at 0.79 (reaching most people through prosecution, hiring discrimination, exile pressure). The grid documents the asymmetry: institutional suppression capacity is higher than grass-level resistance capacity, but organizational and class resistance is substantial and sustained.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Beijing authority) and beneficiary-secondary-payer (Hong Kong establishment) seats compute the constraint very differently from the victim seats. From the Beijing and establishment position, NSL is a coordination mechanism (security, stability, prevention of separatism) with extraction as a side effect or justified cost. From the victim seats (civil society, opposition, press, academics), NSL is pure extraction dressed as security: no genuine coordination function exists because the stated security threats are either vastly exaggerated or fabricated. The engine should compute the Beijing and establishment seats as perceiving something near a tangled_rope (genuine coordination + extraction), while the victim seats perceive a snare (extraction with false coordination cover). This reading is authored from the victim seat(s) perspective: extractiveness and suppression are measured as the victims experience them. The directionality derivation shows: Beijing as full beneficiary (d ≈ 0.05), establishment as partial beneficiary (d ≈ 0.35), civil society and opposition as full targets (d ≈ 0.95), independent press and academics as near-targets (d ≈ 0.90). The massive directionality spread should produce divergent per-seat type classifications, which is the structural truth this constraint embodies: the same legal text functions as security (from Beijing/establishment perspective) and suppression (from victim perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Beijing central authority is beneficiary (full authority consolidation, zero cost incurred by Beijing) → d ≈ 0.0. Hong Kong establishment is beneficiary (consolidates monopoly, removes competitive threat) but carries secondary costs (must administer expanding prosecutions, manages international criticism, incurs identity lock because denial is career-threatening) → d ≈ 0.3–0.4. Civil society organizations are victims (criminalized for lawful speech, prosecution risk, organizational closure) with trapped exit (no alternative organizing venues) → d ≈ 0.95. Opposition parties are victims (electoral participation criminalized, candidates disqualified, fundraising blocked) with trapped exit (no alternative democratic channel) → d ≈ 0.95. Independent press are victims (outlets shuttered, editors arrested, assets frozen, self-censorship enforced) with constrained exit (some journalists can relocate, but this loses on-ground reporting capacity) → d ≈ 0.90. Protest participants are victims (prosecution for assembly, chilling effects) with trapped exit (staying means prosecution risk, leaving means exile) → d ≈ 0.95. Pro-autonomy academics are victims (publication risk, employment risk) with constrained exit (can move to foreign universities, but lose institutional base and research access) → d ≈ 0.88. Independent legal profession are victims (professional coercion, privilege erosion, defense itself framed as subversion) with constrained exit (some lawyers have emigrated, others remain under pressure) → d ≈ 0.85. The directionality override is not required for Beijing or Hong Kong establishment (structural derivation produces correct d) but is offered here: Hong Kong establishment's secondary-role (beneficiary + payer) and identity-locked exit could be overridden to d ≈ 0.25 to reflect the partial extraction they themselves bear, slightly raising their effective χ above the straight beneficiary baseline.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 2019 political unrest over electoral reform and autonomy. The problem status is contested: Beijing/establishment claim it was a security crisis requiring NSL; opposition and international monitors claim it was democratic disagreement requiring democratic resolution. NSL's mandate is ostensibly security: preventing separatism, stopping foreign interference, eliminating terrorism. However, the enforced scope (0.89 extractiveness) is many orders of magnitude broader than any plausible security response to documented threats. The founding problem—preventing genuine security threats—could be addressed through narrowly-targeted legislation criminalizing actual violence or proven foreign espionage. NSL's actual operation criminalizes peaceful protest, electoral opposition, journalism, and scholarship. This is a classic mandatrophy signature: the mandate (security) has outlived or never existed—what persists is the extraction mechanism (political control) wrapped in the mandate's vocabulary. The theater_ratio of 0.62 documents this: security prosecutions are real events (not pure theater), but they are prosecuting conduct that is not plausibly a security threat, creating the appearance of specific charges while the actual function is broad suppression. The divergence between founding problem and actual scope is the mandatrophy vector. A mandatrophy-resolved outcome would either: (1) narrow NSL to genuine security scope (which would drop extractiveness to 0.25–0.35), or (2) acknowledge NSL as a political control instrument and stop dressing it as security (which would raise theater_ratio to 0.85+ as pure political power wielded without security pretense). The current state maintains the mandate fiction while the extraction operates at maximum scope—textbook active mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_emergency_vs_political_control,
    'Is NSL''s scope justified as a proportionate response to a genuine security threat, or does the scope indicate it is a mechanism for permanent political control dressed as security?',
    'Comparative analysis of threat magnitude (documented foreign interference incidents, separatist violent plots) against NSL scope (criminalization of peaceful political speech, opposition organizing, academic research). If the enforced scope is orders of magnitude broader than the documented threat, the justification is false.',
    'If NSL is proportionate security response, the constraint is a tangled_rope (genuine security coordination with extractive side effects). If it is massively over-broad, the security framing is theater and the constraint is a snare with political control as the actual function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_emergency_vs_political_control, empirical, 'Whether NSL''s scope reflects genuine security response or theater covering political enclosure.').

omega_variable(
    vagueness_as_intentional_expansion_vector,
    'Are NSL''s key definitional boundaries (sedition, subversion, foreign collusion, undermining Hong Kong''s political system) intentionally vague to permit expanding prosecution scope over time, or is the vagueness an incidental drafting issue?',
    'Analysis of legal interpretation patterns: if prosecutions systematically expand the boundaries (redefining protest as sedition, normal international engagement as foreign collusion, peaceful advocacy as system undermining), vagueness is functioning as an intentional expansion mechanism. Comparison with comparable security legislation in other jurisdictions (clarity of definitions, scope constraints).',
    'If vagueness is intentional, it is a structural feature enabling ratcheting extraction. The theater ratio would reflect performative adherence to specific charges while the actual suppression mechanism operates through boundary ambiguity. If accidental, the constraint might be reformable through legislative clarification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vagueness_as_intentional_expansion_vector, empirical, 'Whether boundary vagueness serves as an intentional expansion vector for extraction.').

omega_variable(
    reading_alternative_framing_precarity,
    'Could the same NSL legal text be read by a coherent interpreter (with different political commitments than this reading''s author) as a legitimate security instrument (sovereignty_restoration_reading) or as a jurisdictional boundary renegotiation (jurisdictional_capture_reading) rather than as democratic enclosure?',
    'Examine the kernel text''s actual language: does it support multiple readings or primarily one? Interview legal scholars from each reading tradition about what the text requires vs. permits. Compare to competing legality claims from within Hong Kong''s legal profession.',
    'If the text supports multiple readings equally, the reading chosen is partially a normative commitment, not pure textual truth. If one reading is clearly textually dominant, this reading''s framing is the core constraint. This omega documents the inescapable indeterminacy in reading a contested kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_alternative_framing_precarity, conceptual, 'Whether NSL text permits multiple equally-valid readings or admits one dominant reading.').

omega_variable(
    suppression_internalization_dynamic,
    'Is the measured suppression primarily structural (external legal barriers, prosecution risk, institutional barriers) or internalized (self-censorship, self-concept fusion with state authority, terror-based identity shift) or both?',
    'Post-departure trajectories: if people who leave Hong Kong''s jurisdiction continue to self-censor, suppression is internalized. If they resume normal speech, suppression was primarily structural. Longitudinal studies of behavioral change in target populations.',
    'If primarily structural, removal of NSL enforcement would restore democratic space quickly. If internalized, the psychological infrastructure of suppression would persist after legal enforcement ceased, requiring longer psychological and institutional recovery. Theater ratio would reflect different mechanisms (performance for authority vs. internalized fear).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_dynamic, empirical, 'Whether suppression is structural or internalized or both.').

omega_variable(
    kernel_contest_foreclosure,
    'Does the democratic_enclosure_reading''s core claim directly contradict the sovereignty_restoration_reading''s core claim in a way that no single interpretive framework could hold both, or do they coexist as different normative readings of the same text?',
    'Examine: is there a factual claim (about threat magnitude, about 2019 motivations, about judicial independence) that the readings make opposite assertions about? If yes, the readings might foreclose. If the readings accept the same facts but assign different normative weight, they coexist.',
    'Forecloses → one reading''s legitimacy directly eliminates the other within a single framework. Coexists → both readings remain live positions for different institutional actors. This impacts how the engine models the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether this reading forecloses the sovereignty_restoration reading or coexists with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(nsl__tr_t0, observed).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement_basis(nsl__tr_t6, observed).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 12, 0.52).
narrative_ontology:measurement_basis(nsl__tr_t12, observed).
narrative_ontology:measurement(nsl__tr_t18, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 18, 0.57).
narrative_ontology:measurement_basis(nsl__tr_t18, observed).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 24, 0.59).
narrative_ontology:measurement_basis(nsl__tr_t24, observed).
narrative_ontology:measurement(nsl__tr_t30, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 30, 0.61).
narrative_ontology:measurement_basis(nsl__tr_t30, observed).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 36, 0.62).
narrative_ontology:measurement_basis(nsl__tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(nsl__be_t0, observed).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement_basis(nsl__be_t6, observed).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 12, 0.84).
narrative_ontology:measurement_basis(nsl__be_t12, observed).
narrative_ontology:measurement(nsl__be_t18, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 18, 0.87).
narrative_ontology:measurement_basis(nsl__be_t18, observed).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 24, 0.88).
narrative_ontology:measurement_basis(nsl__be_t24, observed).
narrative_ontology:measurement(nsl__be_t30, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(nsl__be_t30, observed).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 36, 0.89).
narrative_ontology:measurement_basis(nsl__be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(nsl__su_t0, observed).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement_basis(nsl__su_t6, observed).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement_basis(nsl__su_t12, observed).
narrative_ontology:measurement(nsl__su_t18, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 18, 0.87).
narrative_ontology:measurement_basis(nsl__su_t18, observed).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement_basis(nsl__su_t24, observed).
narrative_ontology:measurement(nsl__su_t30, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement_basis(nsl__su_t30, observed).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 36, 0.91).
narrative_ontology:measurement_basis(nsl__su_t36, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=36
narrative_ontology:measurement(nsl__grid_01, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(nsl__grid_02, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(class), 36, 0.87).
narrative_ontology:measurement(nsl__grid_03, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(nsl__grid_04, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(individual), 36, 0.82).
narrative_ontology:measurement(nsl__grid_05, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(nsl__grid_06, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(organizational), 36, 0.85).
narrative_ontology:measurement(nsl__grid_07, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(structural), 0, 0.78).
narrative_ontology:measurement(nsl__grid_08, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(structural), 36, 0.88).
narrative_ontology:measurement(nsl__grid_09, nsl_legal_text__democratic_enclosure_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(nsl__grid_10, nsl_legal_text__democratic_enclosure_reading, resistance(class), 36, 0.72).
narrative_ontology:measurement(nsl__grid_11, nsl_legal_text__democratic_enclosure_reading, resistance(individual), 0, 0.45).
narrative_ontology:measurement(nsl__grid_12, nsl_legal_text__democratic_enclosure_reading, resistance(individual), 36, 0.38).
narrative_ontology:measurement(nsl__grid_13, nsl_legal_text__democratic_enclosure_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(nsl__grid_14, nsl_legal_text__democratic_enclosure_reading, resistance(organizational), 36, 0.68).
narrative_ontology:measurement(nsl__grid_15, nsl_legal_text__democratic_enclosure_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement(nsl__grid_16, nsl_legal_text__democratic_enclosure_reading, resistance(structural), 36, 0.35).
narrative_ontology:measurement(nsl__grid_17, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(nsl__grid_18, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(class), 36, 0.86).
narrative_ontology:measurement(nsl__grid_19, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(nsl__grid_20, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(individual), 36, 0.81).
narrative_ontology:measurement(nsl__grid_21, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(organizational), 0, 0.71).
narrative_ontology:measurement(nsl__grid_22, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(organizational), 36, 0.88).
narrative_ontology:measurement(nsl__grid_23, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(structural), 0, 0.65).
narrative_ontology:measurement(nsl__grid_24, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(structural), 36, 0.89).
narrative_ontology:measurement(nsl__grid_25, nsl_legal_text__democratic_enclosure_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement(nsl__grid_26, nsl_legal_text__democratic_enclosure_reading, suppression(class), 36, 0.87).
narrative_ontology:measurement(nsl__grid_27, nsl_legal_text__democratic_enclosure_reading, suppression(individual), 0, 0.55).
narrative_ontology:measurement(nsl__grid_28, nsl_legal_text__democratic_enclosure_reading, suppression(individual), 36, 0.79).
narrative_ontology:measurement(nsl__grid_29, nsl_legal_text__democratic_enclosure_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(nsl__grid_30, nsl_legal_text__democratic_enclosure_reading, suppression(organizational), 36, 0.89).
narrative_ontology:measurement(nsl__grid_31, nsl_legal_text__democratic_enclosure_reading, suppression(structural), 0, 0.72).
narrative_ontology:measurement(nsl__grid_32, nsl_legal_text__democratic_enclosure_reading, suppression(structural), 36, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__democratic_enclosure_reading, 0.12).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% NSL legal text is a contested kernel instantiated in three constraint stories: democratic_enclosure_reading (this story, ε=0.89, snare), sovereignty_restoration_reading (ε≈0.15, mountain-or-rope, security framing), jurisdictional_capture_reading (ε≈0.65, tangled_rope, institutional capture framing). Each reading assesses the same legal text against different referents and structural models, producing different extractiveness values. The three stories form a constraint family linked by network.affects_constraints edges. No reading is privileged; each is a coherent structural analysis from its interpretive seat. The corpus documents the indeterminacy of a kernel by publishing all readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__democratic_enclosure_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
