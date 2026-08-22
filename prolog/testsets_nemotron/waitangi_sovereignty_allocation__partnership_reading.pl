% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty Partnership Reading — Crown-Māori Ongoing Partnership with Consultation Duties
 *   domain: constitutional/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The partnership reading of the Treaty of Waitangi holds that the Treaty
 *   established an ongoing relationship between the Crown and Māori requiring
 *   good faith consultation and active protection of Māori interests, despite
 *   the textual ambiguity between the English and Māori versions. This
 *   reading emerged from the 1975 Treaty of Waitangi Act and was developed
 *   through the Waitangi Tribunal's jurisprudence, the 'principles of the
 *   Treaty' doctrine (from the 1987 Lands case), and the settlement process.
 *   The constraint operates as a tangled_rope: it provides a genuine
 *   coordination function (a framework for Crown-Māori engagement, dispute
 *   resolution, and resource allocation) while simultaneously extracting from
 *   both parties — the Crown cedes some decision-making autonomy, and Māori
 *   accept a process that falls short of tino rangatiratanga and extinguishes
 *   claims through settlements. The 'principles' doctrine constrains Crown
 *   action but explicitly does not override parliamentary sovereignty,
 *   creating a soft constraint whose enforcement depends on political
 *   legitimacy and Māori collective action rather than judicial compulsion.
 *
 * KEY AGENTS:
 *   - maori_collectivities: Primary target and beneficiary (moderate/identity_locked) — bears extraction through settlements that extinguish claims, benefits from consultation rights and settlement redress
 *   - crown_institutions: Agenda setter and beneficiary (institutional/arbitrage) — administers the partnership framework, gains legitimacy and governance certainty, constrains its own sovereignty moderately
 *   - crown_parliamentary_sovereignty_claim: Victim (institutional/analytical) — the partnership duty constrains but does not override parliamentary supremacy; the claim of unlimited sovereignty is the extracted position
 *   - waitangi_tribunal: Observer/agenda_setter (institutional/analytical) — interprets the partnership principle, recommends settlements, but lacks binding enforcement power
 *   - new_zealand_courts: Observer (institutional/analytical) — judicial review of consultation adequacy, declaratory remedies only
 *   - settlement_negotiators: Payer/beneficiary (organized/constrained) — Crown negotiators extract finality; Māori negotiators extract resources but accept extinguishment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.38).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.42).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty Partnership Reading — Crown-Māori Ongoing Partnership with Consultation Duties").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'cdd0793f-e329-4e60-bfc6-7d1ad3dc428a').
narrative_ontology:cs_kernel_codification('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', fixed_text).
narrative_ontology:cs_authority_grounding('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', extraction).
narrative_ontology:cs_interpretation_layer_present('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a').
narrative_ontology:cs_reading_relation('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', foundational, treaty_establishes_ongoing_partnership).
narrative_ontology:cs_axiom_status(treaty_establishes_ongoing_partnership, holdable).
narrative_ontology:cs_axiom_grounding('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', treaty_establishes_ongoing_partnership, conventional).
narrative_ontology:cs_axiom('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', foundational, partnership_constrains_but_does_not_override_parliamentary_sovereignty).
narrative_ontology:cs_axiom_status(partnership_constrains_but_does_not_override_parliamentary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', partnership_constrains_but_does_not_override_parliamentary_sovereignty, conventional).
narrative_ontology:cs_axiom('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', secondary, good_faith_consultation_is_enforceable_duty).
narrative_ontology:cs_axiom_status(good_faith_consultation_is_enforceable_duty, holdable).
narrative_ontology:cs_axiom_grounding('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', good_faith_consultation_is_enforceable_duty, conventional).
narrative_ontology:cs_axiom('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', secondary, active_protection_requires_positive_crown_action).
narrative_ontology:cs_axiom_status(active_protection_requires_positive_crown_action, holdable).
narrative_ontology:cs_axiom_grounding('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', active_protection_requires_positive_crown_action, conventional).
narrative_ontology:cs_reference_frame('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', treaty_partnership_principles_1987).
narrative_ontology:cs_drift_state('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', contemporary_co_governance_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cdd0793f-e329-4e60-bfc6-7d1ad3dc428a', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_collectivities).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_institutions).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_collectivities).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, crown_parliamentary_sovereignty_claim).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, settlement_negotiators_crown).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, settlement_negotiators_maori).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, settlement_negotiators_crown).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, settlement_negotiators_maori).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Māori iwi, hapū, and whānau engage the partnership framework because it is the only constitutional mechanism recognizing Treaty rights. They receive settlement redress and consultation rights but accept Crown-controlled process, quantum, and extinguishment of broader claims. The Treaty relationship is constitutive of their constitutional identity — exit means abandoning the Treaty itself, which is unthinkable for most collectivities. They bear extraction through settlements that deliver 1-3% of estimated losses and extinguish rangatiratanga claims.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_collectivities, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, maori_collectivities, beneficiary).

% The Crown (executive, departments, ministers) designs and administers the partnership framework: defines 'principles', sets settlement policy, controls negotiation mandates, and can legislate to override or modify the framework. It gains legitimacy, governance certainty, and finality of historical claims. Its exit option is legislative override — it could abolish the partnership framework by statute, but the political and international reputational cost is high (arbitrage-grade exit: it has the power to exit but the cost makes it irrational).
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, crown_institutions, beneficiary).

% The doctrinal claim that Parliament retains unlimited legislative sovereignty (Westminster model). The partnership reading explicitly asserts this claim is constrained but not overridden — the partnership duty is a soft constraint that yields to clear parliamentary intent. This claim is 'extracted' in the sense that the partnership doctrine reduces the practical space for unfettered Crown action, but it remains the dominant constitutional framework. As a non-agent (doctrine), it is excluded from directionality derivation but declared as a victim to capture the structural asymmetry.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_parliamentary_sovereignty_claim, payer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(waitangi_sovereignty_allocation__partnership_reading, crown_parliamentary_sovereignty_claim).

% Permanent commission of inquiry established 1975. Investigates Māori claims of Crown breaches, interprets Treaty principles, recommends settlements. Its findings are not binding on the Crown — compliance is political. It provides the authoritative interpretation of the partnership reading but lacks enforcement power. Its exit is analytical: it observes the constraint's operation from outside the extraction flow.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Exercise judicial review of Crown consultation decisions (adequacy, good faith). Remedies are declaratory — courts cannot compel substantive outcomes or invalidate legislation. They define the legal contours of the partnership duty but cannot enforce it against parliamentary sovereignty. Analytical exit: they interpret but do not participate in the extraction.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, new_zealand_courts, observer,
    institutional, generational, analytical, national).

% Crown officials (Office of Treaty Settlements, ministers) who negotiate settlement deeds. They bear the cost of settlement quantum (payer) but gain finality, certainty, and closure of historical claims (beneficiary). Their exit is constrained: they operate within Cabinet mandates and political directives.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, settlement_negotiators_crown, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, settlement_negotiators_crown, beneficiary).

% Māori mandated negotiators (post-settlement governance entities, negotiation teams). They extract settlement resources (beneficiary) but accept Crown process, extinguishment clauses, and outcomes far below rangatiratanga (payer). Their exit is constrained: the mandate comes from their collectivity; walking away means no redress at all.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, settlement_negotiators_maori, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, settlement_negotiators_maori, beneficiary).

% The broader New Zealand public who are not party to Treaty negotiations but are affected by settlement outcomes (resource transfers, co-governance arrangements, legal principles). They are structurally excluded from the partnership framework — not consulted on settlement terms, not represented in co-governance. Their exit is mobile: they can engage politically, emigrate, or disengage. They would object to perceived special privileges or to perceived injustice depending on their view.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, non_maori_new_zealanders, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_institutions).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, ongoing framework for Crown-Māori engagement: consultation processes, dispute resolution (Waitangi Tribunal), and a negotiated settlement process for historical grievances. Solves the coordination problem of how two Treaty partners with asymmetric power and conflicting textual understandings can manage their relationship without either party repudiating the Treaty entirely.
% TRANSFER_FUNCTION: Moves decision-making autonomy from the Crown (constrained by consultation duties) and resource/claims from Māori (extinguished via settlements) into a managed partnership framework. The Crown transfers settlement resources (financial, cultural, governance) to Māori collectivities; Māori transfer the extinguishment of historical claims and acceptance of Crown kāwanatanga. The net flow is bidirectional but asymmetric: Crown retains ultimate legislative authority; Māori receive partial redress.
% ABSENT_VOICES: Māori who reject the partnership frame entirely (advocates for tino rangatiratanga / constitutional transformation) are excluded from the partnership framework — they participate in settlements under protest or not at all. Non-Māori New Zealanders are excluded from settlement negotiations. Future generations of both parties are excluded from the extinguishment decisions made today.
% DISAPPEARANCE_RATIONALE: If the partnership framework vanished overnight: the Treaty would revert to a non-justiciable political promise; the Waitangi Tribunal would lose its jurisdiction; settlement deeds would be the only remaining obligations; Crown would have no legal duty to consult Māori; Māori would lose the only constitutional mechanism for enforcing Treaty guarantees. The Crown-Māori relationship would reorganize around raw power (parliamentary sovereignty) and Māori collective action (protest, international forums, civil disobedience). Co-governance arrangements would lack legal foundation.
% FOUNDING_PROBLEM: The Crown systematically breached the Treaty of Waitangi from 1840 onward (land confiscation, legislative override, failure to protect Māori interests). By 1975, there was no mechanism for Māori to enforce Treaty guarantees or obtain redress for historical grievances. The partnership reading was built to solve: (1) the absence of any enforcement mechanism for Treaty promises, (2) the Crown's refusal to acknowledge breaches, (3) the constitutional vacuum regarding the Treaty's legal status.
% FOUNDING_PROBLEM_CORROBORATION: The Crown (via Treaty of Waitangi Act 1975, settlement policy) attests the founding problem is substantially solved — the partnership framework now provides redress and ongoing engagement. Māori collectivities (via Waitangi Tribunal claims, constitutional transformation advocacy, He Puapua) attest the founding problem persists — the partnership framework manages but does not remedy the core breach (denial of tino rangatiratanga). The Waitangi Tribunal (independent commission) corroborates that historical breaches were severe and that current mechanisms deliver partial but incomplete redress. Independent constitutional scholars (e.g., Moana Jackson, Margaret Mutu) corroborate from outside the Crown's beneficiary set that the partnership frame falls short of Treaty justice.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).
:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate: the partnership constrains Crown decision-making (extraction from Crown sovereignty) while Māori accept a process that delivers less than tino rangatiratanga (extraction from Māori authority). The trajectory shows declining extraction from 1975-2015 as settlements and principles doctrine matured, with a slight uptick to 2025 reflecting co-governance contestation (Three Waters, He Puapua). Suppression (0.42) is moderate: the constraint does not rely on coercive enforcement — the Crown complies largely for legitimacy; Māori participate because the process is the only available redress channel. Theater ratio (0.31) reflects that consultation is often performative (Crown defines 'good faith', timing favors Crown) but settlements deliver real resources. Accessibility collapse (0.52) is moderate: alternatives (rangatiratanga, crown_sovereignty) remain live in discourse. Resistance (0.58) is significant: Māori resist the partnership frame as insufficient; Crown resists constraints on sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's institutional seat, the partnership is a genuine coordination mechanism that provides governance certainty, legitimacy, and a managed path to resolve historical grievances — experienced as a rope with moderate self-imposed constraint. From Māori collectivities' seat, the same structure extracts tino rangatiratanga through a process the Crown controls (settlement terms, quantum, extinguishment) while delivering partial redress — experienced as a tangled_rope or snare depending on the settlement. From the parliamentary sovereignty claim seat, the partnership is an illegitimate constraint on Crown power — experienced as extraction without coordination benefit. The engine computes these divergences from the structural data: Crown has arbitrage-grade exit (could legislate away the partnership), Māori are identity_locked (Treaty relationship is constitutive), parliamentary sovereignty claim is analytical (no exit, no direct extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Māori collectivities are both beneficiaries (consultation rights, settlement resources, institutional recognition) and victims (extinguishment of broader claims, process controlled by Crown, outcomes below rangatiratanga). Crown institutions are agenda setters (design the process, define 'principles', control settlement quantum) and beneficiaries (legitimacy, governance certainty, finality). The parliamentary sovereignty claim is a victim — the partnership doctrine constrains Crown discretion. The Waitangi Tribunal and courts are observers with analytical exit. Directionality for Māori is pulled toward target (identity_locked exit, high stakes) despite beneficiary status; for Crown it is pulled toward beneficiary (institutional power, arbitrage exit) despite bearing some constraint. The structural asymmetry: Crown controls the process design and can ultimately override via legislation; Māori cannot exit the Treaty relationship without losing its constitutional foundation.
 *
 * MANDATROPHY ANALYSIS:
 *   The partnership reading was founded to solve the problem of Crown breaches of the Treaty and the absence of any mechanism for Māori to enforce Treaty guarantees (founding_problem: live/contested — historical breaches are acknowledged but whether current partnership mechanisms adequately address them is contested). The arrangement has not atrophied — it has expanded (settlements, co-governance, principles in legislation). However, the mandate has shifted: from 'remedy breaches' to 'manage the Crown-Māori relationship within Crown sovereignty'. The partnership reading does not suffer mandatrophy in the piton sense (it is not a degraded institution maintained theatrically), but it does face a legitimacy crisis: Māori increasingly argue the partnership frame has become a ceiling on rangatiratanga rather than a floor. The constraint persists because both parties are invested in it (Crown for legitimacy/certainty, Māori for what redress it delivers) but neither sees it as fully adequate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (waitangi_sovereignty_allocation), and how does the partnership reading''s structural profile differ from its siblings?',
    'Compare the constraint profiles of all three readings (partnership_reading, crown_sovereignty_reading, rangatiratanga_reading) on extractiveness, suppression, beneficiaries/victims, and directionality. The kernel_id and reading_id are declared in commentary.kernel_context.',
    'If the partnership reading shows moderate extraction with bidirectional beneficiaries/victims while siblings show asymmetric extraction profiles, the kernel decomposition is structurally validated. If all three readings collapse to similar profiles, the decomposition may be rhetorical rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment of this constraint to the kernel-reading frame; structural distinctness of the partnership reading from its siblings').

omega_variable(
    partnership_vs_rangatiratanga_foreclosure,
    'Does the partnership reading''s core premise (ongoing Crown-Māori partnership with shared authority) logically foreclose the rangatiratanga reading''s premise (Māori retained full authority over lands/resources/taonga), or do they coexist as competing frameworks?',
    'Examine whether a single legal framework could simultaneously hold: (a) Crown and Māori as partners with shared decision-making, and (b) Māori as retaining tino rangatiratanga (full authority) over their domains. If partnership requires Crown consent where rangatiratanga requires Māori consent alone, they may foreclose. If partnership is interpreted as the Crown''s kāwanatanga operating alongside rangatiratanga, they may coexist.',
    'If forecloses: the two readings cannot both be holdable in one framework; one must be overridden or the kernel splits. If coexists_with: both remain live positions in NZ constitutional discourse, held by different parties. If influences: partnership reading''s institutionalization (settlements, principles) creates structural pressure on rangatiratanga claims without resolving them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_vs_rangatiratanga_foreclosure, conceptual, 'Structural relationship between partnership_reading and rangatiratanga_reading within the waitangi_sovereignty_allocation kernel').

omega_variable(
    partnership_vs_crown_sovereignty_coexistence,
    'Does the partnership reading coexist with the crown_sovereignty_reading (Westminster parliamentary supremacy), or does one foreclose the other?',
    'Examine whether parliamentary sovereignty (Crown''s unlimited legislative power) can coexist with a binding partnership duty requiring good faith consultation and active protection. The partnership reading asserts the duty constrains but does not override parliamentary sovereignty — this is a coexistence claim. Test whether courts and political actors in fact treat the partnership duty as a genuine constraint on Crown power or as a principle that yields whenever Parliament clearly intends otherwise.',
    'If coexists_with: the partnership reading operates as a soft constraint within a sovereign framework — real but non-entrenched. If forecloses: either partnership entails a limit Parliament cannot override (which the reading explicitly denies), or crown_sovereignty is false. If influences: partnership doctrine creates legitimacy pressure on Crown actions without legally binding Parliament.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partnership_vs_crown_sovereignty_coexistence, conceptual, 'Structural relationship between partnership_reading and crown_sovereignty_reading').

omega_variable(
    consultation_enforcement_mechanism,
    'What enforces the consultation duty — judicial review, political accountability, or Māori collective action? The answer determines whether the constraint is a genuine coordination mechanism (rope-like) or extraction dressed as partnership (snare-like).',
    'Track enforcement outcomes: when Crown fails consultation, what remedies follow? Judicial declarations without coercive remedies = weak enforcement. Political cost = moderate. Māori withdrawal of cooperation = strong. The pattern of actual enforcement over the interval determines the constraint''s operational type.',
    'If enforcement depends on Māori collective action (withdrawal of legitimacy, protest, international forums), the constraint''s extraction profile is bidirectional — Crown extracts decision-making freedom, Māori extract accountability. If enforcement is purely judicial with declaratory remedies only, the partnership may be performative (higher theater_ratio). If Crown unilaterally defines ''good faith'', the constraint extracts from Māori without reciprocal constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultation_enforcement_mechanism, empirical, 'Enforcement mechanism of the partnership consultation duty and its effect on the constraint''s operational classification').

omega_variable(
    settlement_as_extraction_or_redress,
    'Do Treaty settlements function as genuine redress (coordination: restoring Māori capacity) or as a managed extraction channel (Crown controls quantum, timing, and terms, extinguishing broader claims)?',
    'Analyze settlement terms: quantum relative to loss, Crown control of process, whether settlements extinguish rangatiratanga claims or only specific grievances, and whether settlements increase or decrease Māori decision-making authority over remaining resources.',
    'If settlements are Crown-managed extraction: they appear in victims for Māori (extinguishment of broader claims) and beneficiaries for Crown (finality, certainty). If settlements are genuine redress: they appear in beneficiaries for Māori (resource restoration) and the constraint''s extractiveness decreases over time. The trajectory of base_extractiveness measurements over the interval tests this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_as_extraction_or_redress, empirical, 'Whether Treaty settlements operate as coordination (redress) or extraction (managed finality)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waitangi_partnership_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.65).
narrative_ontology:measurement(waitangi_partnership_tr_t1985, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1985, 0.52).
narrative_ontology:measurement(waitangi_partnership_tr_t1995, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(waitangi_partnership_tr_t2005, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2005, 0.31).
narrative_ontology:measurement(waitangi_partnership_tr_t2015, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(waitangi_partnership_tr_t2025, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(waitangi_partnership_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(waitangi_partnership_be_t1985, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(waitangi_partnership_be_t1995, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(waitangi_partnership_be_t2005, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(waitangi_partnership_be_t2015, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement(waitangi_partnership_be_t2025, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(waitangi_partnership_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.72).
narrative_ontology:measurement(waitangi_partnership_su_t1985, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1985, 0.61).
narrative_ontology:measurement(waitangi_partnership_su_t1995, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(waitangi_partnership_su_t2005, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(waitangi_partnership_su_t2015, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(waitangi_partnership_su_t2025, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__partnership_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, treaty_settlement_process).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, maori_co_governance_arrangements).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, principles_of_treaty_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the waitangi_sovereignty_allocation constraint family (kernel). The three readings (partnership, crown_sovereignty, rangatiratanga) decompose the single natural-language concept 'the Treaty of Waitangi' into structurally distinct constraints with different ε values, beneficiary/victim structures, and operational types. The partnership reading has moderate bidirectional extraction (tangled_rope); crown_sovereignty_reading likely shows low extraction from Crown/high from Māori (snare); rangatiratanga_reading likely shows high extraction from Crown/low from Māori (rope or scaffold from Māori seat). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, moderate, 0.68).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
