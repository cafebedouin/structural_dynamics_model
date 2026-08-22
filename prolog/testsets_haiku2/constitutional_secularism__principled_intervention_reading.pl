% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Constitutional Secularism: Principled Intervention Reading
 *   domain: constitutional_law/religious_governance
 *
 * SUMMARY:
 *   This constraint represents one reading of the constitutional secularism
 *   kernel — the principled-intervention reading. The reading holds that
 *   state may and should intervene in religious affairs when justified by
 *   social reform objectives and protection of marginalized community
 *   members. The constraint instantiates this reading as a structural
 *   arrangement: state authority is expanded into religious governance on
 *   grounds of advancing equality and dignity; religious organizations lose
 *   autonomy; marginalized members gain legal remedies at the cost of state
 *   intrusion into intimate religious spaces. The claim/metric gap is
 *   deliberate and diagnostic: the reading CLAIMS tangled_rope (genuine
 *   coordination of equality rights with religious participation), while the
 *   authored metrics describe substantially extractive operation (0.68
 *   extractiveness) with rising suppression of religious autonomy claims. The
 *   engine measures that divergence; do not reconcile the claim to the
 *   metrics. The constraint is evaluated from THIS reading's epistemic
 *   position — what counts as oppression, what makes intervention principled,
 *   what reform objectives legitimate state power — all read through this
 *   reading's lights.
 *
 * KEY AGENTS:
 *   - constitutional_state: institutional agenda-setter, expanded authority into religious governance, justified by reform mandate
 *   - marginalized_community_members: powerless beneficiaries, identity-locked (cannot exit religious identity), gain legal remedies but at cost of state intrusion
 *   - reform_advocates: organized beneficiaries, mobile (not trapped in religious system), shape what counts as oppressive and thus subject to intervention
 *   - religious_organizations: powerful payers, lose autonomy over governance and practice, suppressed or delegitimized when claiming autonomous authority
 *   - traditionalist_practitioners: moderate payers, constrained exit (religious identity constituted), face legal restriction and authority diminishment
 *   - strict_neutrality_advocates: excluded (present in litigation but delegitimized), contend intervention violates genuine secular neutrality
 *   - constitutional_courts: observers, adjudicate disputes, set effective boundaries of state power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.68).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.71).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Constitutional Secularism: Principled Intervention Reading").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '0f0205f1-af03-404d-87ae-81dffe4fb137').
narrative_ontology:cs_kernel_codification('0f0205f1-af03-404d-87ae-81dffe4fb137', formalized).
narrative_ontology:cs_authority_grounding('0f0205f1-af03-404d-87ae-81dffe4fb137', extraction).
narrative_ontology:cs_interpretation_layer_present('0f0205f1-af03-404d-87ae-81dffe4fb137').
narrative_ontology:cs_reading_relation('0f0205f1-af03-404d-87ae-81dffe4fb137', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f0205f1-af03-404d-87ae-81dffe4fb137', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('0f0205f1-af03-404d-87ae-81dffe4fb137', foundational, state_reformist_authority_justified).
narrative_ontology:cs_axiom_status(state_reformist_authority_justified, holdable).
narrative_ontology:cs_axiom_grounding('0f0205f1-af03-404d-87ae-81dffe4fb137', state_reformist_authority_justified, deontological).
narrative_ontology:cs_axiom('0f0205f1-af03-404d-87ae-81dffe4fb137', foundational, marginalized_member_protection_overrides_autonomous_authority).
narrative_ontology:cs_axiom_status(marginalized_member_protection_overrides_autonomous_authority, holdable).
narrative_ontology:cs_axiom_grounding('0f0205f1-af03-404d-87ae-81dffe4fb137', marginalized_member_protection_overrides_autonomous_authority, deontological).
narrative_ontology:cs_reference_frame('0f0205f1-af03-404d-87ae-81dffe4fb137', equal_constitutional_citizenship_through_state_safeguards).
narrative_ontology:cs_drift_state('0f0205f1-af03-404d-87ae-81dffe4fb137', contemporary_religious_pluralism_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0f0205f1-af03-404d-87ae-81dffe4fb137', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, reform_advocates).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, marginalized_community_members).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_organizations).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, traditionalist_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises constitutional authority to intervene in religious institutions on grounds of advancing social reform and protecting marginalized groups. Interprets religious autonomy as conditional on conformity to constitutional equality and dignity principles. Reserves the right to regulate internal religious practices when reform objectives and marginalized protection are at stake. Operates the enforcement machinery that suppresses competing constitutional interpretations (strict neutrality, narrow autonomy).
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_state, agenda_setter,
    institutional, generational, analytical, national).

% Members of communities within religious institutions (lower-caste members in hierarchical religions, women in patriarchal structures, sexual minorities in traditional communities) who face discrimination within their religious community. State intervention creates formal equality claims and legal remedies. They benefit from recognition, but the benefit is contingent on state power being deployed into intimate religious spaces they cannot exit due to familial and cultural ties.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, marginalized_community_members, beneficiary,
    powerless, biographical, identity_locked, national).

% Civil society organizations, feminist and social-justice advocates, legal reformers, and intellectuals who frame religious institutions as sites of systematic oppression. Benefit from state authority being expanded to reach into religious governance and internal practices. Shape what counts as oppressive and thus subject to state intervention. Operate in the public sphere and can exit to other social movements; not trapped in the religious system being reformed.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, reform_advocates, beneficiary,
    organized, generational, mobile, national).

% Institutional actors (temples, mosques, churches, monasteries and their governing bodies) that lose autonomy over internal governance, membership criteria, ritual practices, ordination/leadership, and discipline to state regulation justified by reform objectives. Cannot exit the regulatory regime; can only litigate or resist politically. Religious authority traditions that grounded legitimacy in unquestioned internal sovereignty face constant incursion and must defend autonomy claims against the charge of perpetuating oppression.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_organizations, payer,
    powerful, generational, constrained, national).

% Adherents and practitioners (clergy, scholars, ritual specialists, committed believers) who hold religious practices and internal structures as authoritative according to their tradition's own lights. Face legal restriction on practices designated as oppressive, pressure to conform to secular constitutional principles, and diminishment of religious authority in favor of state-appointed adjudication. Exit means religious abandonment, which is identity-constituted for many. Bear the suppressive weight directly without organizational power to contest.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, traditionalist_practitioners, payer,
    moderate, biographical, constrained, national).

% Constitutional scholars, civil libertarians, and religious minority leaders who argue the constraint violates genuine secular neutrality by privileging majoritarian reform agendas over minority religious autonomy. Contend that 'principled intervention' is a cover for state-enabled majoritarianism. Present in constitutional litigation and political debate, but their positions are systematically delegitimized by reform narratives and institutional arrangements that treat principled intervention as the settled reading.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, strict_neutrality_advocates, excluded,
    organized, generational, constrained, national).

% Sit between the state's reform mandate and religious organizations' autonomy claims. Adjudicate disputes, interpret constitutional provisions, and set the effective boundary of state power. Operate within the constraint's authority framework but their doctrinal choices amplify or restrain intervention scope. Courts' rulings determine which cases count as principled intervention and which count as overreach.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__principled_intervention_reading, constitutional_state).
narrative_ontology:fixing_cost_class(constitutional_secularism__principled_intervention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single framework by which internal religious practices are assessed against constitutional principles of equality, non-discrimination, and dignity, rather than leaving such practices to autonomous religious governance with no external standard. Solves the coordination problem of how to adjudicate conflicts between religious autonomy and constitutional rights without fragmenting into plural incompatible legal regimes.
% TRANSFER_FUNCTION: Transfers authority over internal religious governance (over membership criteria, leadership selection, discipline, ritual practice, resource allocation) from religious organizations and traditionalist practitioners to the constitutional state and reform advocates. Religious organizations pay in autonomy lost; marginalized members and reform advocates receive legal authority to reshape religious institutions. The state captures the power to define which practices are oppressive and thus governable.
% ABSENT_VOICES: Strict-neutrality advocates and religious minority leaders who argue this reading violates genuine secular neutrality. They would contend that majoritarian reform agendas are embedded in the reading's interpretation of secularism itself, and that authentic neutrality requires the state to maintain equal distance from all religions. Indigenous and minority religious communities whose internal structures do not conform to majoritarian institutional models (matrilineal inheritance, elder councils, collective decision-making) are often unheard because reform standards presume dominant institutional forms.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, religious organizations would regain autonomous authority over their governance, membership, leadership, and discipline. Marginalized members would lose formal legal remedies for discrimination within religious spaces and would be thrown back on intra-religious contestation or exit. Reform advocates would lose the institutional power to reshape religious institutions by law. The constitutional interpretation of secularism would reorganize around either strict neutrality or a different reading of principled intervention.
% FOUNDING_PROBLEM: Religious institutions historically maintained hierarchies of caste, gender, sexual orientation, and other marginalized statuses as integral to their authority structures and legitimacy traditions. These institutions controlled resources, education, social belonging, and ritual participation, making exit costly or impossible for marginalized members. The founding problem: how can constitutionalism protect the dignity and equality of marginalized people when religious institutions that control their access to community claim the right to govern their internal affairs autonomously?
% FOUNDING_PROBLEM_CORROBORATION: Reform advocates, marginalized community members, and some constitutional courts attest the founding problem is live — religious institutions continue to marginalize members through caste, gender, sexual-orientation, and other discriminations built into authority structures and practices. Religious organizations and traditionalist practitioners attest the founding problem is misframed — what reformers call oppression they frame as internal spiritual authority and boundary maintenance that has been substantially reformed through internal efforts and is no longer systematically enforced through exclusion. Constitutional courts in India, Egypt, Israel, and elsewhere show deep division: some jurisdictions support the reformist framing of the founding problem as ongoing; others recognize religious autonomy framing the problem as substantially addressed by social change. Legal scholars outside both benefiting parties (reformers) offer conflicting assessments: some support state intervention to protect marginalized members; others argue genuine neutrality requires the state to maintain equal distance. No single corroborating source outside the benefiting parties unambiguously establishes the founding problem in a way that traditionalist seats would accept.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68 at interval end) and rising because state power to regulate religious governance is exercised asymmetrically: beneficiaries (reform advocates, marginalized members) gain authority to shape religious practice; payers (religious organizations, traditionalist practitioners) lose autonomy without exit. The extraction is not simple rent-collection but authority-capture — the state monopolizes the right to define which religious practices are oppressive and thus governable. Suppression is high (0.71) and rising because the constraint's persistence depends on actively delegitimizing or suppressing the strict-neutrality framing (that state should maintain equal distance from all religions). Suppression is the enforcement machinery that keeps the principled-intervention reading hegemonic and forecloses alternative readings from becoming institutionalized. Theater is moderate (0.52) and rising: the constraint is justified through sincere reform rhetoric, but as enforcement expands, an increasing proportion of regulatory activity defends the reading's hegemony (suppressing competing constitutional interpretations) rather than protecting marginalized members directly. The measurements show extraction and suppression rising together (t=0 to t=25), then theater rising to dominate (t=25 to t=40) — consistent with a constraint whose initial coordination function (protecting marginalized members) is increasingly supplemented by performative defense of its own doctrinal authority.
 *
 * PERSPECTIVAL GAP:
 *   The state and reform advocates sit on the coordination side: they experience the constraint as generating genuine authority to protect equality and dignity; they see it solving a real coordination problem (how to adjudicate conflicts between religious autonomy and constitutional rights). Religious organizations and traditionalist practitioners sit on the extraction side: they experience the constraint as an asymmetric loss of autonomy, justified by external (majoritarian) reform standards imposed without their agreement. Strict-neutrality advocates occupy a third position: they contend both sides misread secularism — that genuine neutrality requires the state to maintain equal distance from all religions, making this constraint a violation of its own pretended foundation. The engine should compute three different type classifications from one seat each: the state/reform advocates should compute toward rope-or-scaffold (coordination frame dominates their experience); religious organizations should compute toward snare (extraction frame dominates theirs); strict-neutrality advocates should compute toward snare (the constraint suppresses their reading of what secularism means). This divergence is the key analytical finding — not a defect in the constraint, but evidence of irreducible perspectival asymmetry in constitutional secularism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from beneficiary/victim positioning plus exit. The constitutional state sits at the agenda-setter position: it controls the interpretation and enforcement machinery; d approaches 0.0 (beneficiary of its own authority expansion, though states do not 'benefit' in the sense of collecting extraction — their benefit is authority itself). Reform advocates are mobile beneficiaries (d near 0.2): they benefit from expanded state power into religious governance; they can exit to other social movements if this reading loses; they are not trapped in the arrangement. Marginalized community members are identity-locked beneficiaries (d near 0.35): they benefit from legal remedies, but their identity is fused with the religious community being regulated; they cannot exit religion itself; the benefit comes at the cost of state intrusion into intimate spaces. Religious organizations are powerful payers (d near 0.8): they lose autonomy; they can contest the reading legally and politically but cannot exit the regulatory regime. Traditionalist practitioners are moderate, identity-locked payers (d near 0.85): they lose authority; they cannot exit their religion; exit would mean religious abandonment. Strict-neutrality advocates are trapped-exit payers (d near 0.9): they view the constraint as a violation of the constitutional structure itself; they can contest it in courts but cannot exit the constitutional order; their contestation is systematically delegitimized.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint carries a real mandatrophy risk. The founding problem was: religious institutions perpetuate discrimination against marginalized members and these institutions control belonging and resources, making exit costly. The constraint claims to solve this by empowering state intervention on reform grounds. But the founding problem status is contested: are religious hierarchies still operative oppressive structures, or have they been substantially reformed by social change? Traditionalist practitioners and some religious scholars argue the latter — that internal religious reforms have addressed the worst practices, and what remains are legitimately plural ways of organizing religious community. If the founding problem has substantially died while the constraint persists, the constraint becomes a vehicle for state control of religion justified by an obsolete narrative. The theater_ratio rising toward 0.54 at t=40 suggests this dynamic: enforcement shifts from protecting marginalized members (a live function) to defending the reading's hegemony against strict-neutrality and reformist alternative readings (a performative function). The measurement trajectory is consistent with a constraint undergoing mandatrophy: real coordination function (protecting vulnerable people within religious institutions) degrading into mostly-theatrical authority assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_problem_persistence,
    'Has the founding problem (systematic religious oppression of marginalized members) substantially persisted, or has internal reform and social change largely resolved it?',
    'Ethnographic and legal-historical evidence from religious institutions over a generational span: are marginalized members still systematically excluded, subordinated, or denied resources based on caste/gender/other status within religious governance and practice?',
    'If the founding problem is substantially dead, the constraint undergoes mandatrophy: it persists as enforcement machinery without a live coordination function. The divergence between reform-advocate framing (oppression is live) and traditionalist framing (oppression is addressed) becomes the constraint''s core instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_persistence, empirical, 'Whether the founding problem (religious oppression of marginalized members) remains live or has been substantially resolved.').

omega_variable(
    principled_vs_majoritarian_capture,
    'Does state intervention genuinely follow ''principled'' boundaries (protecting only against practices that demonstrably oppress marginalized members) or does ''principled'' function as cover for majoritarian reform agendas unrelated to marginalized protection?',
    'Comparative institutional analysis: compare state intervention into religious practices reformers target with intervention into religious practices marginalized members identify as oppressive. Do they overlap substantially or is intervention driven by reformer agendas?',
    'If principled boundaries hold, the tangled_rope framing is more defensible (genuine coordination of marginalized protection with reform oversight). If capture occurs, the constraint should reclassify toward snare (extraction of religious autonomy justified by reform rhetoric).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(principled_vs_majoritarian_capture, empirical, 'Whether ''principled intervention'' remains bounded by marginalized protection or becomes a vehicle for majoritarian religious reform agendas.').

omega_variable(
    reading_foreclosure_structural_inevitability,
    'Does this reading (principled intervention) necessarily foreclose the strict-neutrality reading, or are they coexisting alternative positions that a constitutional order could hold in tension?',
    'Doctrinal and institutional analysis: can a single constitutional order recognize both principled-intervention authority and strict-neutrality constraints on that authority (e.g., through judicial skepticism applied case-by-case) without internal contradiction?',
    'If they necessarily foreclose each other, the constraint embeds a choice between readings into constitutional structure, making suppression of strict-neutrality an inevitable component of the arrangement. If they can coexist, suppression is not structural but political, and the high suppression_requirement (0.71) becomes a key diagnostic finding of majoritarian power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structural_inevitability, conceptual, 'Whether this reading necessarily forecloses strict neutrality or whether both can coexist in constitutional doctrine.').

omega_variable(
    state_authority_expansion_reversibility,
    'Is the expansion of state authority into religious governance reversible through constitutional amendment or doctrinal shift, or does institutional lock-in make the expansion durable regardless of later readings'' preferences?',
    'Historical-institutional analysis: examine whether similar authority expansions in other constitutional systems proved reversible or whether they locked in through bureaucratic entrenchment, judicial precedent, and political investment.',
    'High reversibility supports the tangled_rope reading (the arrangement could be renegotiated if the reading shifted). Low reversibility suggests the constraint functions as a ratchet — beneficiaries gain irreversible authority that later courts or readings cannot undo, making the arrangement asymmetrically binding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_authority_expansion_reversibility, empirical, 'Whether the state authority expansion is reversible or irreversibly locked in by institutional forces.').

omega_variable(
    alternative_readings_suppression_mechanism,
    'What institutional mechanisms suppress or delegitimize the strict-neutrality and reformist readings, and are those mechanisms intrinsic to this reading or contingent political choices?',
    'Institutional ethnography of constitutional courts, legal education, and civil-society discourse: what constraints do legal professionals operate under that make principled-intervention framing default and alternative readings marginal?',
    'If suppression is intrinsic (this reading''s logic necessarily excludes alternatives), suppression is structural. If suppression is contingent (political choices to elevate this reading), suppression is removable, and high suppression_requirement signals political contestation rather than logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_suppression_mechanism, conceptual, 'Whether suppression of alternative readings is structurally necessary or contingently political.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cons_tr_t5, constitutional_secularism__principled_intervention_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__principled_intervention_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__principled_intervention_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__principled_intervention_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__principled_intervention_reading, theater_ratio, 25, 0.53).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__principled_intervention_reading, theater_ratio, 30, 0.54).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__principled_intervention_reading, theater_ratio, 40, 0.51).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cons_be_t5, constitutional_secularism__principled_intervention_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__principled_intervention_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__principled_intervention_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__principled_intervention_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__principled_intervention_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__principled_intervention_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__principled_intervention_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cons_su_t5, constitutional_secularism__principled_intervention_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__principled_intervention_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__principled_intervention_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__principled_intervention_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__principled_intervention_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__principled_intervention_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__principled_intervention_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__principled_intervention_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_secularism kernel. Sibling constraints: strict_neutrality_reading (state maintains equal distance from all religions; no preferential intervention) and reformist_reading (state has affirmative duty to eliminate oppressive religious practices, superseding autonomy claims entirely). The three readings share the same kernel commitment (secular governance) but instantiate different ε values and beneficiary/victim structures. Principled-intervention reading: state authority expanded on grounds of reform and marginalized protection (0.68 extractiveness from religious organizations' perspective; coordination from reform advocates' perspective). Strict-neutrality reading: state authority constrained to maintain equal distance (lower extractiveness if enforced; higher if principled-intervention reading dominates and suppresses it). Reformist reading: state authority expanded without principled constraint (potentially higher extractiveness than principled intervention, as it does not claim to limit itself). Do not merge these readings into one constraint; the framework's job is to register their structural divergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
