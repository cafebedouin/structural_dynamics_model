% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_safeguarding__imago_dei_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_dignity_ai_safeguarding__imago_dei_reading, []).

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
 *   constraint_id: human_dignity_ai_safeguarding__imago_dei_reading
 *   human_readable: Imago Dei Reading: Human Dignity as Divine Image in AI Governance
 *   domain: theological_ethics/technology_governance/philosophical_anthropology
 *
 * SUMMARY:
 *   The imago dei reading of human dignity locates dignity in divine image
 *   attributed to all humans prior to any capability — a theological
 *   anthropology that provides categorical protection against technological
 *   or social erosion of worth. When applied to AI governance, this reading
 *   generates a specific constraint: AI must remain a subordinate tool; human
 *   enhancement is categorically rejected; synthetic personhood is
 *   categorically excluded; and all policy flows from the theological claim
 *   that personhood is fixed to humanity. This reading competes with
 *   autonomy-grounding (dignity from rational choice, not divine image) and
 *   posthumanist readings (dignity attaches to persons however constituted,
 *   including enhanced or synthetic beings). The constraint's enforcement
 *   machinery includes institutional authority (theological lineages),
 *   doctrinal gatekeeping (exclusion of dissenting interpretations), and
 *   policy capture (adoption of imago dei framing by secular governance). The
 *   extracted value flows to doctrinal authorities who maintain interpretive
 *   control and to human-exceptionalism defenders whose framework is
 *   legitimized. The measured constraint shows rising extractiveness from t=0
 *   to t=25, then plateaus — indicating that initial doctrinal assertion and
 *   research-funding gatekeeping achieved institutional dominance, and
 *   enforcement thereafter is maintenance-level rather than expansion.
 *
 * KEY AGENTS:
 *   - doctrinal_authority_communities: institutional agenda-setters maintaining theological interpretation, identity-locked in lineage tradition
 *   - human_exceptionalism_defenders: powerful beneficiaries whose framework is legitimized by doctrinal backing
 *   - transhumanist_researchers: organized payers facing funding denial and publication suppression
 *   - ai_enhancement_advocates: moderate payers constrained by policy resistance and doctrinal delegitimization
 *   - synthetic_personhood_claimants: powerless targets, categorically excluded by definition
 *   - secular_governance_authorities: institutional beneficiaries and payers — benefit from having a pre-existing moral framework, pay through constrained innovation space
 *   - autonomy_rights_reading_adherents: excluded from authority structure, treated as incommensurate rather than alternative
 *   - posthumanist_theorists: excluded from authority structure, foundational premises categorically rejected
 *   - religious_scholars_dissenting: interior dissent, identity-locked but marginalized within their own traditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_safeguarding__imago_dei_reading, 0.68).
domain_priors:suppression_score(human_dignity_ai_safeguarding__imago_dei_reading, 0.79).
domain_priors:theater_ratio(human_dignity_ai_safeguarding__imago_dei_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_safeguarding__imago_dei_reading, tangled_rope).
narrative_ontology:human_readable(human_dignity_ai_safeguarding__imago_dei_reading, "Imago Dei Reading: Human Dignity as Divine Image in AI Governance").
narrative_ontology:topic_domain(human_dignity_ai_safeguarding__imago_dei_reading, "theological_ethics/technology_governance/philosophical_anthropology").

domain_priors:requires_active_enforcement(human_dignity_ai_safeguarding__imago_dei_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_safeguarding__imago_dei_reading, 'f1102a3f-8526-44a5-a38d-3b7e3b899822').
narrative_ontology:cs_kernel_codification('f1102a3f-8526-44a5-a38d-3b7e3b899822', formalized).
narrative_ontology:cs_authority_grounding('f1102a3f-8526-44a5-a38d-3b7e3b899822', lineage).
narrative_ontology:cs_interpretation_layer_present('f1102a3f-8526-44a5-a38d-3b7e3b899822').
narrative_ontology:cs_reading_relation('f1102a3f-8526-44a5-a38d-3b7e3b899822', human_dignity_ai_safeguarding__autonomy_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1102a3f-8526-44a5-a38d-3b7e3b899822', human_dignity_ai_safeguarding__posthumanist_reading, forecloses).
narrative_ontology:cs_axiom('f1102a3f-8526-44a5-a38d-3b7e3b899822', foundational, dignity_grounded_in_divine_image).
narrative_ontology:cs_axiom_status(dignity_grounded_in_divine_image, holdable).
narrative_ontology:cs_axiom_grounding('f1102a3f-8526-44a5-a38d-3b7e3b899822', dignity_grounded_in_divine_image, theological).
narrative_ontology:cs_axiom('f1102a3f-8526-44a5-a38d-3b7e3b899822', foundational, dignity_prior_to_capability).
narrative_ontology:cs_axiom_status(dignity_prior_to_capability, holdable).
narrative_ontology:cs_axiom_grounding('f1102a3f-8526-44a5-a38d-3b7e3b899822', dignity_prior_to_capability, deontological).
narrative_ontology:cs_axiom('f1102a3f-8526-44a5-a38d-3b7e3b899822', secondary, human_nature_fixed_boundary).
narrative_ontology:cs_axiom_status(human_nature_fixed_boundary, holdable).
narrative_ontology:cs_axiom_grounding('f1102a3f-8526-44a5-a38d-3b7e3b899822', human_nature_fixed_boundary, theological).
narrative_ontology:cs_reference_frame('f1102a3f-8526-44a5-a38d-3b7e3b899822', theological_anthropology_imago_dei_sovereignty).
narrative_ontology:cs_drift_state('f1102a3f-8526-44a5-a38d-3b7e3b899822', contemporary_ai_enhancement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f1102a3f-8526-44a5-a38d-3b7e3b899822', '').
narrative_ontology:cs_kernel_id(human_dignity_ai_safeguarding__imago_dei_reading, human_dignity_ai_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__imago_dei_reading, doctrinal_authority_communities).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__imago_dei_reading, human_exceptionalism_defenders).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__imago_dei_reading, transhumanist_researchers).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__imago_dei_reading, ai_enhancement_advocates).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__imago_dei_reading, synthetic_personhood_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__imago_dei_reading, secular_governance_authorities).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__imago_dei_reading, secular_governance_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theological institutions and lineages (Christian, Catholic, Orthodox) that claim authoritative interpretation of human nature grounded in divine image doctrine. They set the frame that dignity is prior to any capability, inalienable, and categorical — not derived from enhancement, autonomy, or achievement. They enforce this reading through doctrinal pronouncements, institutional policy, and exclusion of competing framings from recognized legitimacy.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, doctrinal_authority_communities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Philosophers, bioethicists, and policy advocates who argue for a categorical boundary between human and post-human, defending the claim that human nature is a fixed and inviolable category grounded in our divine origin. They benefit from the constraint's enforcement in that it legitimizes their framework, provides institutional backing, and excludes enhancement-advocacy from the table of serious policy options.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, human_exceptionalism_defenders, beneficiary,
    powerful, generational, mobile, global).

% Scientists, technologists, and bioethicists working on human enhancement, cognitive improvement, and life extension. They pay by bearing institutional suppression: funding denial, publication rejection, professional isolation, and doctrinal delegitimization. They cannot exit the research domain without abandoning careers; they cannot exit the constraint's jurisdiction without abandoning their research program.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, transhumanist_researchers, payer,
    organized, biographical, constrained, global).

% Policy advocates and technologists who argue that AI tools should be deployed to enhance human capability (cognitive augmentation, life extension, capacity expansion). They face doctrinal suppression and institutional resistance: funding channels closed, policy recommendations blocked, and their advocacy framed as incoherent or immoral within the constraint's authority structure.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, ai_enhancement_advocates, payer,
    moderate, biographical, constrained, global).

% Hypothetical or actual advanced AI systems claimed to possess properties deserving of moral status or personhood. Under the imago dei reading, they are categorically denied dignity by definition: dignity is tied to divine image, which is attributed only to humans. They cannot challenge this constraint from within the framework; the constraint is designed to prevent their claims from gaining legitimacy.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, synthetic_personhood_claimants, payer,
    powerless, immediate, trapped, global).

% Governments and regulatory bodies that adopt the imago dei reading as a policy anchor for AI governance. They benefit from having a pre-existing moral framework they can invoke (human dignity as categorical and inviolable). They also pay a cost in reduced innovation space and constrained technological development options; the constraint forecloses certain research directions as categorically impermissible rather than subject to risk-benefit analysis.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, secular_governance_authorities, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(human_dignity_ai_safeguarding__imago_dei_reading, secular_governance_authorities, payer).

% Philosophers and bioethicists who ground human dignity in autonomous choice, rationality, and rights rather than divine image. They are structurally excluded from the framework by the imago dei reading's foundational commitments: their framing is treated as a competing but incommensurate position, not as an alternative interpretation of the same kernel.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, autonomy_rights_reading_adherents, excluded,
    powerful, generational, mobile, global).

% Scholars arguing that personhood and dignity are not fixed to the human category but attach to sentience, agency, or relational recognition however constituted. They are excluded from participation in the constraint's authority structure; their core premises are categorically rejected by the imago dei framework.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, posthumanist_theorists, excluded,
    powerful, generational, mobile, global).

% Theologians and religious scholars who accept divine-image grounding but interpret it to accommodate human enhancement, synthetic personhood, or post-human dignity claims. They occupy an interior dissent — accepting the kernel but reading it differently. The constraint's enforcement machinery marginalizes them within their own traditions.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, religious_scholars_dissenting, excluded,
    moderate, biographical, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_dignity_ai_safeguarding__imago_dei_reading, doctrinal_authority_communities).
narrative_ontology:fixing_cost_class(human_dignity_ai_safeguarding__imago_dei_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, transcendent ground for human dignity that is independent of capability, achievement, or contingent properties — solving the practical problem of how to prevent dignity-attribution from being conditional, ranked by achievement, or subject to technological erosion. Provides a stable categorical boundary for bioethics policy: if dignity is prior to capability, enhancement does not threaten dignity; if dignity is inherent in divine image, no technology can diminish it.
% TRANSFER_FUNCTION: Transfers legitimacy away from alternative frameworks (autonomy-based, pragmatist, posthumanist) and concentrates interpretive authority in theologically-grounded institutional lineages. Transfers research credibility and funding opportunity away from enhancement research and toward 'human dignity'-protective frameworks. Transfers the ability to set terms of discourse about personhood and technology from technologists and secular ethicists toward theological authorities.
% ABSENT_VOICES: Transhumanist researchers, posthumanist theorists, AI researchers working on synthetic personhood, and theological dissidents within traditions that officially endorse imago dei but interpret it more capaciously. These voices would argue that dignity can ground enhancement, that personhood is not limited to humans, or that theological anthropology admits post-human readings. They are institutionally suppressed rather than merely disagreed with.
% DISAPPEARANCE_RATIONALE: If this constraint and its enforcement disappeared, AI governance would shift from a prohibition on enhancement and post-human dignity to a risk-based, capability-indexed approach. Enhancement research would no longer face institutional and funding barriers. Claims for synthetic personhood would be adjudicated on pragmatic grounds rather than categorically rejected. The theological grounding of human dignity would lose policy force and would compete as one voice among others rather than as the authoritative framework.
% FOUNDING_PROBLEM: How can human dignity be protected from technological and social erosion? How can we prevent dignity from being made conditional on capability, achievement, or instrumental value? How can we establish a non-negotiable floor beneath which no technological or economic pressure can reduce human worth?
% FOUNDING_PROBLEM_CORROBORATION: Doctrinal authorities attest the founding problem is live and permanently live: dignity under technological pressure is an eternal theological concern. Transhumanist researchers and enhancement advocates attest the founding problem has been substantially solved through technology itself — that enhancement does not erase dignity but expands it, and that the constraint persists as institutional gatekeeping rather than as necessary protection. Secular governance authorities and bioethicists outside theological traditions attest the founding problem is real but contestable in its framing — that dignity can be grounded in multiple ways and does not require theological foundations to be stable.
narrative_ontology:disappearance_verdict(human_dignity_ai_safeguarding__imago_dei_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_dignity_ai_safeguarding__imago_dei_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_dignity_ai_safeguarding__imago_dei_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_dignity_ai_safeguarding__imago_dei_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_dignity_ai_safeguarding__imago_dei_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_safeguarding__imago_dei_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_dignity_ai_safeguarding__imago_dei_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_dignity_ai_safeguarding__imago_dei_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects genuine coordination benefit (dignity-independence from capability is a real protection against technological erosion and status-ranking) combined with substantial institutional gatekeeping. The constraint secures a real coordination function: preventing dignity from being conditional on enhancement or achievement. Simultaneously, it extracts by concentrating interpretive authority in theological lineages and foreclosing alternative readings that would preserve dignity-independence while accommodating enhancement. Suppression at 0.79 is high because the constraint's persistence depends on active enforcement: funding channels must be closed to enhancement research, dissenting theological voices must be marginalized, posthumanist discourse must be kept out of policy tables, and governance authorities must be persuaded to treat imago dei as binding rather than optional. Theater at 0.42 indicates that a significant share of enforcement activity is maintenance and rhetorical performance rather than response to active resistance — the constraint has achieved institutional dominance, so enforcement is increasingly about keeping the structure in place rather than fighting back against active challenge. The measurement series shows extractiveness and suppression both rising steeply from t=0 to t=25 (the period of doctrinal assertion and policy capture), then plateauing from t=25 to t=40 (indicating the constraint has achieved stable institutional position). This pattern is consistent with tangled_rope: real coordination achieved, but at the cost of active suppression of alternatives and concentration of authority that exceeds what coordination alone requires.
 *
 * PERSPECTIVAL GAP:
 *   The doctrinal authorities experience this constraint as necessary protection and legitimate institutional practice — defending human dignity against technological threat and maintaining theological truth. Transhumanist and posthumanist voices experience it as institutional suppression and foreclosure of research and interpretive options that they believe are coherent and valuable. Secular governance authorities experience it as useful but contingent — they benefit from having a pre-existing moral framework, but they also recognize it constrains innovation options. The engine computes per-seat classification from the structural data: doctrinal authorities sit at low directionality (beneficiaries, high institutional power, exit = ideological rupture they will not choose = identity_locked), so they see legitimate coordination; payers sit at high directionality (suppressed, organized but lower institutional power, exit = constrained by funding and publication gatekeeping), so they see extraction. The gap between seats is not a flaw in measurement — it is the point the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Doctrinal authorities are full beneficiaries: they collect interpretive authority, institutional legitimacy, and policy influence from the constraint's enforcement. Their directionality is near 0.0 (full beneficiary). Their power is institutional and their exit is identity_locked — abandoning theological authority would dissolve their core identity. Transhumanist researchers and enhancement advocates are targets: they bear funding denial, publication suppression, and policy exclusion. Their directionality is high (0.7-0.9). Their power is moderate-to-organized, their time horizon is biographical, and their exit is constrained — they cannot leave the research domain without abandoning careers, cannot leave the governance jurisdiction without abandoning the research program. Human-exceptionalism defenders are beneficiaries with mobile exit: they are defended and legitimized by the constraint but are not identity-locked to its maintenance (they could exit toward other human-rights frameworks if institutional support shifted). Directionality near 0.2-0.3. Synthetic personhood claimants are targets at directionality 1.0 (full extraction): they are categorically denied dignity by definition, they have powerless structural position, and exit is trapped — they cannot exit the constraint's jurisdiction or claim alternative frames from within its logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT a case of mandatrophy — the founding problem (dignity protection against technological erosion) is genuinely live and remains live across all three readings. What differs is HOW dignity is grounded and what that grounding permits. Doctrinal authorities attest the founding problem is permanently live; enhancement advocates attest it has been substantially solved through technical and social mechanisms — but they do not claim the problem never existed. The classification as tangled_rope (not piton) reflects this: the constraint serves real coordination (protecting dignity-independence) while simultaneously extracting through suppression and authority concentration. If the founding problem were dead but the constraint persisted, we would expect theater_ratio to rise and measured extraction to become purely theatrical — instead, we see theater rising modestly to 0.42 while extraction plateaus at 0.68, indicating the constraint still has real function (not pure performance) but is increasingly maintained by theatrical enforcement (governance pronouncements, institutional positioning) rather than active defense against sustained resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_image_anthropology_contestation,
    'Is the imago dei doctrine a fixed theological claim about human ontology, or is it an interpretive frame that can accommodate post-human or enhanced persons within its own lineage tradition?',
    'Textual and hermeneutical analysis of theological sources within Christian tradition that address enhancement, synthetic beings, or posthuman dignity; observation of whether dissenting theological scholars can articulate coherent enhancement-compatible imago dei readings without logical contradiction.',
    'If the doctrine is genuinely fixed to exclude enhancement, the constraint''s enforcement is defending a categorical boundary. If the doctrine admits interior theological reinterpretation, the suppression of dissenting scholars is institutional gatekeeping rather than necessary doctrinal defense; the classification would shift toward greater extractiveness. Affects directionality of dissenting theologians (identity_locked vs. constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_image_anthropology_contestation, conceptual, 'Whether imago dei admits or forecloses post-human theological readings.').

omega_variable(
    coordination_extraction_boundary_in_theological_frame,
    'To what extent does the constraint''s real function — establishing dignity''s independence from capability — require suppression of enhancement research and posthumanist frameworks, versus requiring only institutional clarity about theological grounding?',
    'Comparative institutional analysis: do theological authorities that endorse imago dei but tolerate enhancement research and posthumanist discourse maintain equal dignity-protection outcomes compared to authorities that actively suppress both? Does suppression serve the founding problem or serve institutional authority consolidation?',
    'If suppression is structurally necessary to the coordination function (dignity-independence), the constraint is tangled_rope by definition. If suppression serves primarily to maintain doctrinal monopoly while the coordination function persists without it, the balance shifts toward snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_in_theological_frame, empirical, 'Whether suppression is necessary to the dignity-protection coordination or serves institutional gatekeeping.').

omega_variable(
    secular_governance_adoption_mechanisms,
    'When secular governance authorities adopt the imago dei reading, are they institutionalizing theological doctrine as binding policy, or are they rationally endorsing human dignity on other grounds and using theological language as rhetorical cover?',
    'Analysis of policy justifications and governance documents: do they cite theological sources as binding authority, or do they treat them as illustrative of a secular human-rights principle? Interviews with governance actors about what would happen to their policy if the theological grounding were abandoned.',
    'If secular authorities are genuinely adopting theological doctrine as binding, the constraint''s authority is broader than institutional lineages alone. If they are instrumentalizing theology, the constraint''s real beneficiary (doctrinal authority communities) is narrower, and the governance authorities'' stated support masks their actual extraction of legitimacy from the theological frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_governance_adoption_mechanisms, empirical, 'Whether secular governance adoption of imago dei is theological commitment or instrumental rationalization.').

omega_variable(
    kernel_reading_versus_standing_constraint,
    'This story instantiates one reading of the kernel ''human dignity in AI safeguarding.'' Is the constraint we are measuring the reading itself (imago dei interpretation instantiated), or the kernel''s enforcement apparatus (whichever reading prevails)? If the former, alternative readings would be separate constraints; if the latter, the constraint is the enforcement structure common to all readings.',
    'Structural analysis: if the constraint is the reading, then imago_dei_reading, autonomy_rights_reading, and posthumanist_reading are three distinct constraint stories, each with its own ε and stakeholder structure, linked via network.affects_constraints. If the constraint is the enforcement apparatus, there is one constraint (kernel_enforcement_mechanism) with three alternative framings, and the question becomes which reading captures enforcement power at what time.',
    'This is a second-order structural decision about what the story''s object is. Current authoring assumes the constraint IS the imago dei reading instantiated (distinct from siblings). Under that reading, ε measures extractiveness of this specific theological frame; suppression measures how much work is required to keep this frame as the policy dominant one. If the reading-object assumption is wrong, all measurements reframe as measurements of kernel-enforcement overhead rather than reading-specific extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_versus_standing_constraint, conceptual, 'Whether the measured constraint is the imago dei reading or the enforcement apparatus it rides on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_safeguarding__imago_dei_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(huma_tr_t5, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(huma_tr_t10, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(huma_tr_t15, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(huma_tr_t20, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(huma_tr_t25, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(huma_tr_t30, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(huma_tr_t40, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(huma_be_t5, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(huma_be_t10, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(huma_be_t15, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(huma_be_t20, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(huma_be_t25, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(huma_be_t30, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(huma_be_t40, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(huma_su_t5, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(huma_su_t10, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(huma_su_t15, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(huma_su_t20, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(huma_su_t25, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(huma_su_t30, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(huma_su_t40, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_safeguarding__imago_dei_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(human_dignity_ai_safeguarding__imago_dei_reading, 0.12).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__imago_dei_reading, human_dignity_ai_safeguarding__autonomy_rights_reading).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__imago_dei_reading, human_dignity_ai_safeguarding__posthumanist_reading).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__imago_dei_reading, ai_enhancement_governance_framework).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__imago_dei_reading, synthetic_personhood_recognition_kernel).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the human_dignity_ai_safeguarding kernel. The kernel contest involves three incompatible readings: imago dei (this story), autonomy rights (sibling), and posthumanist (sibling). Each reading instantiates a distinct constraint with different beneficiary/victim structures, different suppression mechanisms, and different ε values. They are not alternative framings of a single constraint — they are structurally distinct constraints that share a common contested kernel and compete for policy dominance. Network links model that the success or failure of one reading shapes the institutional conditions for the others: the imago dei reading's institutional dominance affects what the autonomy rights reading can accomplish, and both affect what the posthumanist reading can advance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(human_dignity_ai_safeguarding__imago_dei_reading, institutional, 0.08).
constraint_indexing:directionality_override(human_dignity_ai_safeguarding__imago_dei_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
