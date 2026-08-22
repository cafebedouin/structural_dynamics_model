% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__democratic_pluralist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: AI Governance Legitimacy via Democratic Pluralist Deliberation
 *   domain: political_theology/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel 'AI
 *   governance legitimacy'—specifically, the democratic pluralist reading
 *   that derives legitimacy from inclusive deliberation and denies any single
 *   tradition (ecclesiastical, technocratic, market) monopoly authority over
 *   governance principles. The reading accepts religious voices (including
 *   Catholic teaching) as legitimate contributors but rejects the claim that
 *   the Magisterium uniquely interprets human dignity or that technical
 *   optimization or market efficiency can override deliberatively-authored
 *   rules. Extracted cost falls on those excluded from deliberative processes
 *   and populations under authoritarian regimes where this reading's
 *   mechanisms do not apply. The constraint is temporary
 *   scaffolding—participatory infrastructure designed to transition toward
 *   stable, inclusive governance once deliberative norms are
 *   institutionalized. The reading's core axiom
 *   (democratic_deliberation_supremacy) asserts that democratic processes
 *   with broad participation and judicial rights-protection are more
 *   legitimate than expert or hierarchical pronouncement; this coexists with
 *   but does not foreclose the magisterial and technocratic readings, each of
 *   which could be held by different institutional parties in different
 *   governance domains.
 *
 * KEY AGENTS:
 *   - civil_society_organizations: mobilize constituencies; frame pluralism as requirement for legitimacy
 *   - democratic_institutions: author rules via electoral accountability; claim primary authority
 *   - minority_rights_holders: depend on deliberative inclusion; lack independent enforcement power
 *   - excluded_deliberative_participants: bear costs of exclusion; structurally absent from forums
 *   - populations_under_authoritarian_regimes: excluded by non-democratic governance; global constraint victims
 *   - ecclesiastical_hierarchy: positioned as one voice, contest their unique interpretive authority
 *   - technical_expertise_communities: contributors, not sole authorities; contest optimization dominance
 *   - market_actors: subject to democratic governance, not autonomous; contest exit-based legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.42).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.38).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "AI Governance Legitimacy via Democratic Pluralist Deliberation").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "political_theology/technology_governance").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, '821ce2ed-ae8c-476d-9434-aa768d018f86').
narrative_ontology:cs_kernel_codification('821ce2ed-ae8c-476d-9434-aa768d018f86', distributed).
narrative_ontology:cs_authority_grounding('821ce2ed-ae8c-476d-9434-aa768d018f86', distributed).
narrative_ontology:cs_reading_relation('821ce2ed-ae8c-476d-9434-aa768d018f86', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('821ce2ed-ae8c-476d-9434-aa768d018f86', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('821ce2ed-ae8c-476d-9434-aa768d018f86', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('821ce2ed-ae8c-476d-9434-aa768d018f86', foundational, democratic_deliberation_supremacy).
narrative_ontology:cs_axiom_status(democratic_deliberation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('821ce2ed-ae8c-476d-9434-aa768d018f86', democratic_deliberation_supremacy, deontological).
narrative_ontology:cs_axiom('821ce2ed-ae8c-476d-9434-aa768d018f86', foundational, no_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('821ce2ed-ae8c-476d-9434-aa768d018f86', no_interpretive_monopoly, conventional).
narrative_ontology:cs_reference_frame('821ce2ed-ae8c-476d-9434-aa768d018f86', pluralist_democratic_legitimacy).
narrative_ontology:cs_drift_state('821ce2ed-ae8c-476d-9434-aa768d018f86', contemporary_ai_governance_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('821ce2ed-ae8c-476d-9434-aa768d018f86', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, excluded_deliberative_participants).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobilize constituencies to participate in deliberative processes, commission research on AI ethics, and testify before regulatory bodies. They frame AI governance as requiring broad public voice and defend the legitimacy of democratic processes over technocratic or ecclesiastical monopolies. They benefit from the constraint's participatory architecture and invest in maintaining it.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary).

% Parliaments, regulatory agencies, and elected oversight bodies that author and enforce AI governance rules through public deliberation. They claim legitimacy through electoral accountability and transparent rulemaking. Under this reading they are the primary authority structure for AI governance decisions, not religious hierarchies or technical elites.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Groups whose interests are systematically underrepresented in tech design and deployment (religious minorities, marginalized communities, linguistic minorities, disabled persons). They benefit when deliberative processes include their voices and judicial review protects their rights against majoritarian or technocratic override. They depend on the constraint's guarantee of meaningful participation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    powerless, biographical, constrained, national).

% Communities and individuals structurally absent from deliberative forums due to language barriers, economic precarity, geographic isolation, or political suppression. They bear the costs of AI governance decisions made without their voice — algorithmic discrimination, surveillance, loss of cultural autonomy — while the constraint's participatory infrastructure excludes them in practice despite its formal universality.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, excluded_deliberative_participants, payer,
    powerless, biographical, trapped, local).

% Populations in non-democratic states where this reading's deliberative mechanisms do not apply and are actively suppressed. They bear the cost of AI governance legitimated only within liberal-democratic frameworks — their own governance remains autocratic or colonized by external technocratic impositions. They are the constraint's primary victims globally.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes, payer,
    powerless, biographical, trapped, global).

% The Magisterium and Catholic social teaching institutions. Under this reading, they are explicitly positioned as ONE voice among many in deliberative processes, not as unique interpreters of human dignity or governance principles. They would argue (from the magisterial_subsidiarity_reading) that Catholic principles have binding authority independent of secular democratic processes and that subsidiarity requires institutional church authority over implementation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, ecclesiastical_hierarchy, excluded,
    institutional, civilizational, constrained, global).

% AI researchers, engineers, and technical governance specialists. Under this reading they are contributors to deliberation, not the sole authority. They would argue (from the technocratic_optimization_reading) that technical feasibility and performance constraints must override deliberative consensus when they conflict, and that optimization metrics should dominate value pluralism.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technical_expertise_communities, excluded,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, technical_expertise_communities, observer).

% Private technology companies and investors. Under this reading, their conduct is subject to democratically authored governance, not autonomous market discipline. They would argue (from the market_libertarian_reading) that deliberative constraints on AI deployment constitute illegitimate coercion and that competitive markets and exit options better protect dignity than centralized democratic mandate.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, market_actors, excluded,
    powerful, biographical, arbitrage, global).

% Courts and constitutional bodies that review AI governance decisions for consistency with rights, procedural fairness, and democratic values. They enforce the constraint by invalidating governance decisions that exclude voices or violate procedural legitimacy, creating feedback loops that strengthen the participatory infrastructure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, judicial_review_bodies, agenda_setter,
    institutional, generational, constrained, national).

% The procedural machinery through which democratic deliberation is grounded in accountability: elections, recalls, public comment periods, legislative debate, town halls. Not an actor but a structural commitment the reading depends on to distinguish itself from technocratic and ecclesiastical readings.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, electoral_accountability_mechanisms, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(ai_governance_legitimacy__democratic_pluralist_reading, electoral_accountability_mechanisms).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__democratic_pluralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assembles diverse stakeholders—civil society, technical experts, affected communities, elected representatives—into deliberative forums where AI governance principles are authored through transparent debate and consensus-building. Solves the coordination problem of legitimizing binding AI governance rules when no single tradition has monopoly authority and affected populations are geographically and culturally dispersed.
% TRANSFER_FUNCTION: Transfers authority over AI governance principles FROM technocratic elites, ecclesiastical hierarchies, and unaccountable market actors TO democratically accountable institutions and inclusive deliberative processes. Moves legitimacy FROM expert pronouncement or doctrinal authority TO public reason and electoral accountability.
% ABSENT_VOICES: Populations under authoritarian regimes, linguistic minorities excluded from deliberative forums due to language choice, economically precarious communities unable to participate in time-intensive deliberation, indigenous peoples whose knowledge systems are not recognized in formal deliberative structures, future generations whose interests cannot be represented. These absences are the constraint's primary limitation and source of contestation.
% DISAPPEARANCE_RATIONALE: If this deliberative constraint vanished, AI governance would realign toward either technocratic optimization (technical elites set rules), ecclesiastical authority (religious hierarchies impose principles), or market discipline (companies self-regulate). Democratic institutions would lose their claim to set governance principles; civil society mobilization would lose its institutional anchor. The global fragmentation into competing governance regimes (some ecclesiastical, some market-driven, some autocratic) would deepen.
% FOUNDING_PROBLEM: Early AI development was governed by technical experts and corporate leadership with no deliberative input from affected communities, religious traditions, or democratic institutions. AI principles (alignment, fairness, transparency) were authored in technical conferences and corporate ethics boards, not in public forums. Democratic societies lacked mechanisms to exercise collective voice over AI governance.
% FOUNDING_PROBLEM_CORROBORATION: Civil society organizations, democratic theorists, and minority rights advocates attest the founding problem is live: AI governance remains substantially elite-driven despite increased deliberative infrastructure. Technical communities and market actors attest the problem is overstated or solved through existing market competition and professional ethics norms. No corroborating source outside the civil-society beneficiary set verifies the founding problem's current status; this is a central ambiguity the constraint leaves unresolved.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).
:- end_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the reading operates as coordination—it solves a real problem (who legitimates binding AI governance when no single tradition holds monopoly)—while simultaneously extracting from excluded populations by making deliberative participation the legitimacy criterion they cannot meet. Suppression is lower (0.38) than extraction because this reading's enforcement relies primarily on electoral accountability and judicial review rather than coercive exclusion; however, suppression rises when populations are structurally unable to participate (language barriers, economic precarity, authoritarianism). Theater rises over the interval (0.08 → 0.22) as deliberative forums proliferate but actual decision-making authority concentrates, creating the appearance of pluralism while governance remains elite-coordinated. Accessibility_collapse is moderate because alternatives exist (exit to market governance, ecclesiastical governance, technocratic expert rule) but are politically suppressed within liberal-democratic states; for populations outside those states, collapse is near-total. Resistance is high (0.61) because excluded populations actively contest the constraint, democratic opposition parties challenge the reading's procedural fairness, and ecclesiastical and market actors dispute its legitimacy. The coercion_grid shows class-level suppression (0.42 → 0.48) rising as AI governance stakes rise, while individual-level suppression remains lower (0.18 → 0.24) because procedural rights exist even for excluded participants; organizational resistance is highest (0.62 → 0.68) because institutions can coordinate challenge to the constraint's legitimacy claims.
 *
 * PERSPECTIVAL GAP:
 *   From the civil-society and democratic-institution seats, this constraint is coordination: it distributes authority across multiple voices, prevents monopoly capture, and builds legitimate governance. From the excluded and authoritarian-regime seats, it is extraction: they bear costs of governance rules they have no voice in making and are simultaneously blamed for non-participation (if they cannot afford time, lack language access, or live under suppression). From the ecclesiastical seat, it is snare: the constraint positions Catholic teaching as one voice among many, denying the Magisterium's claim to interpret dignity authoritatively—extraction of interpretive authority under the guise of pluralism. From the market seat, it is also snare: deliberative mandates override market discipline and autonomous exchange, extracting efficiency and exit options under the guise of public reason. The engine computes per-seat classification from directionality and the shared structural data; this reading-relative extraction is the core feature that makes the kernel contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation chains from beneficiary/victim declarations through exit_options and power atoms. Civil society organizations: declared beneficiaries, organized power, mobile exit (can establish counter-forums elsewhere), d near 0.2. Democratic institutions: declared beneficiaries (coordinate rule-setting authority), institutional power, constrained exit (lose legitimacy if they exit democratic process), d ≈ 0.35. Minority rights holders: declared beneficiaries (formal inclusion principle) but also declared victims (structural exclusion), powerless, constrained exit (cannot opt out of governance), d ≈ 0.65 (victim status dominates due to exit constraints). Excluded deliberative participants: declared victims, powerless, trapped exit (no alternative governance forums available), d ≈ 0.88. Populations under authoritarian regimes: declared victims, powerless, trapped exit, d ≈ 0.92. Ecclesiastical hierarchy: not declared as beneficiary (the reading denies their benefit) but structurally loses authority; institutional power, constrained exit, d ≈ 0.75 (target end: they pay via lost interpretive monopoly). No directionality overrides needed; derivation from declared structural data captures the reading-relative extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—lack of deliberative democratic input into early AI governance—is declared 'contested' in status. Civil society and democratic institutions attest it remains live; technical and market actors attest it is overstated or solved. The constraint is classified as Scaffold precisely to resolve this mandatrophy ambiguity: it is temporary infrastructure whose sunset depends on whether the founding problem is solved (legitimate, inclusive governance infrastructure becomes stable) or is unsolvable (participation remains structurally limited and the scaffold becomes permanent performance). If the problem is dead (AI governance is genuinely pluralistic), the scaffold sunset is justified and the constraint exits cleanly. If the problem is live (deliberation remains exclusionary), the scaffold will fail to sunset and will degrade into Piton (participatory theater maintained for legitimacy while decisions concentrate). The coercion_grid shows theater rising (0.08 → 0.22) which is consistent with Piton-ward drift if the sunset fails; this is the measurement the corpus uses to detect mandatrophy resolution. A rising theater_ratio without corresponding accessibility_collapse decrease would signal the constraint is becoming performance without substance—evidence that the founding problem is unsolved and the scaffold is degrading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberative_inclusion_frontier,
    'Which populations currently excluded from deliberative AI governance processes (economically precarious, linguistically marginal, geographically remote, politically suppressed) represent the binding constraint on the reading''s legitimacy claim?',
    'Systematic audit of participation in AI governance forums (parliaments, regulatory bodies, public comment periods, civil society networks) disaggregated by language, economic status, geography, political regime. Compare stated inclusion principles against measured participation.',
    'If exclusions are random/accidental, the constraint is legitimacy-building; if systematic/structural, the constraint''s claim to democratic pluralism becomes self-referential (plurality among the included only) and the extracted cost from excluded populations rises. Scaffold status depends on whether the sunset clause can realistically foreclose remaining exclusions or whether they are permanent structural features.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deliberative_inclusion_frontier, empirical, 'Whether measured deliberative participation matches the reading''s inclusive-pluralism claim.').

omega_variable(
    kernel_reading_distinctiveness,
    'Does this reading''s core claim—that AI governance legitimacy derives from democratic deliberation with no single tradition holding interpretive monopoly—logically foreclose the magisterial_subsidiarity_reading, or can both readings coexist as held by different institutional actors?',
    'Formal analysis of the two readings'' axioms: if one asserts the Magisterium has binding interpretive authority over dignity-derived principles and this reading asserts no single tradition has such authority, they foreclose. If this reading only claims democratic institutions have authority WITHIN secular frameworks while the magisterial reading claims authority WITHIN Catholic institutional structures, they coexist (different reference frames). The resolution hinges on whether the kernels compete within the same framework or across frameworks.',
    'If foreclosure: this reading eliminates the magisterial reading from any unified governance architecture; one must be chosen. If coexistence: both readings remain live as held by different institutional parties (Catholic governance structures, democratic nation-states) and the constraint is one reading among coexistent alternatives, not a unique legitimate form. The constraint''s claimed universality would be falsified under coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctiveness, conceptual, 'Whether readings of this kernel are logically incompatible or institutionally coexistent.').

omega_variable(
    sunset_clause_viability,
    'What conditions would trigger the sunset clause—when does the scaffolding transition end and stable governance infrastructure take over? Is the endpoint defined, attainable, and recognizable?',
    'Explicit specification of the exit criterion: e.g., ''when 80% of global AI governance operates under deliberatively-authored rules with documented participation from affected communities'' or ''when authoritarian regimes democratize'' or ''never—perpetual participatory maintenance is the endpoint.'' Check whether the criterion is measurable and whether any actor has structural incentive to delay or accelerate the transition.',
    'If the exit criterion is vague or requires political conditions (regime change) outside the constraint''s control, the scaffold becomes piton-like: performed participation without real sunsetting. If the criterion is achievable but asymmetrically benefits the agenda-setter (democratic institutions capture governance before relinquishing scaffolding), the reading''s commitment to pluralism is undermined and it operates as Tangled Rope (coordination + extraction) rather than pure Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_viability, empirical, 'Whether the scaffold has a defined, viable exit condition or operates as permanent infrastructure.').

omega_variable(
    religious_voice_epistemic_status,
    'In this reading''s deliberative framework, what epistemic status does religious reasoning (including Catholic social teaching) hold? Is it one evidence type among many, a special voice on dignity questions, or systematically downweighted as ''sectarian''?',
    'Examine how deliberative forums weight testimony from religious actors and doctrinal reasoning: are they granted equal standing with secular philosophy, economic analysis, and technical expertise? Is there structural bias toward secular reasoning? How do the reading''s procedural rules handle value pluralism that includes theological claims?',
    'If religious reasoning is systematically downweighted, the reading violates its own pluralism claim and operates as secular-rationalist dominance masked by deliberative procedure—a snare targeting excluded theological traditions. If religious reasoning is genuinely equal-weighted, the reading''s coexistence with magisterial_subsidiarity is strengthened (both operate within shared deliberative space); if segregated (religious consultation without power), the scaffold benefits civil society organizations that coordinate with secular institutions and extracts from marginalized religious communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_voice_epistemic_status, conceptual, 'Whether pluralist deliberation genuinely includes theological reasoning or relegates it to marginalized input.').

omega_variable(
    sibling_reading_incompleteness,
    'This constraint is one of four readings of the ai_governance_legitimacy kernel. Is the sibling-set complete—are there other readings (e.g., indigenous-sovereignty-based, postcolonial, feminist) that the manifest has not named?',
    'Systematic review of governance legitimacy claims made by actors OUTSIDE the four-reading set: indigenous governments, feminist theorists, postcolonial scholars, marginalized religious traditions. If they articulate structurally distinct positions on what legitimizes AI governance, add them as distinct constraints and update the kernel network.',
    'If the sibling set is incomplete, the corpus underrepresents the actual plurality of readings this kernel admits. This reading''s claim to ''no single tradition holds monopoly'' becomes hollow if the corpus omits major traditions. The manifested constraint family should map the actual contested space, not just elite deliberative positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_incompleteness, empirical, 'Whether the four-reading sibling set exhausts the actual readings of the ai_governance_legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(ai_g_tr_t0, observed).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement_basis(ai_g_tr_t4, observed).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(ai_g_tr_t8, observed).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(ai_g_tr_t12, observed).
narrative_ontology:measurement(ai_g_tr_t18, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(ai_g_tr_t18, projected).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(ai_g_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(ai_g_be_t0, observed).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement_basis(ai_g_be_t4, observed).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement_basis(ai_g_be_t8, observed).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(ai_g_be_t12, observed).
narrative_ontology:measurement(ai_g_be_t18, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 18, 0.43).
narrative_ontology:measurement_basis(ai_g_be_t18, projected).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(ai_g_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(ai_g_su_t0, observed).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement_basis(ai_g_su_t4, observed).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement_basis(ai_g_su_t8, observed).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement_basis(ai_g_su_t12, observed).
narrative_ontology:measurement(ai_g_su_t18, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 18, 0.39).
narrative_ontology:measurement_basis(ai_g_su_t18, projected).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(ai_g_su_t25, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(ai_g_grid_01, ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(ai_g_grid_02, ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse(class), 25, 0.61).
narrative_ontology:measurement(ai_g_grid_03, ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(ai_g_grid_04, ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse(individual), 25, 0.42).
narrative_ontology:measurement(ai_g_grid_05, ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(ai_g_grid_06, ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse(organizational), 25, 0.52).
narrative_ontology:measurement(ai_g_grid_07, ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse(structural), 0, 0.44).
narrative_ontology:measurement(ai_g_grid_08, ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse(structural), 25, 0.48).
narrative_ontology:measurement(ai_g_grid_09, ai_governance_legitimacy__democratic_pluralist_reading, resistance(class), 0, 0.73).
narrative_ontology:measurement(ai_g_grid_10, ai_governance_legitimacy__democratic_pluralist_reading, resistance(class), 25, 0.78).
narrative_ontology:measurement(ai_g_grid_11, ai_governance_legitimacy__democratic_pluralist_reading, resistance(individual), 0, 0.44).
narrative_ontology:measurement(ai_g_grid_12, ai_governance_legitimacy__democratic_pluralist_reading, resistance(individual), 25, 0.51).
narrative_ontology:measurement(ai_g_grid_13, ai_governance_legitimacy__democratic_pluralist_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(ai_g_grid_14, ai_governance_legitimacy__democratic_pluralist_reading, resistance(organizational), 25, 0.68).
narrative_ontology:measurement(ai_g_grid_15, ai_governance_legitimacy__democratic_pluralist_reading, resistance(structural), 0, 0.55).
narrative_ontology:measurement(ai_g_grid_16, ai_governance_legitimacy__democratic_pluralist_reading, resistance(structural), 25, 0.62).
narrative_ontology:measurement(ai_g_grid_17, ai_governance_legitimacy__democratic_pluralist_reading, stakes_inflation(class), 0, 0.53).
narrative_ontology:measurement(ai_g_grid_18, ai_governance_legitimacy__democratic_pluralist_reading, stakes_inflation(class), 25, 0.58).
narrative_ontology:measurement(ai_g_grid_19, ai_governance_legitimacy__democratic_pluralist_reading, stakes_inflation(individual), 0, 0.31).
narrative_ontology:measurement(ai_g_grid_20, ai_governance_legitimacy__democratic_pluralist_reading, stakes_inflation(individual), 25, 0.38).
narrative_ontology:measurement(ai_g_grid_21, ai_governance_legitimacy__democratic_pluralist_reading, stakes_inflation(organizational), 0, 0.42).
narrative_ontology:measurement(ai_g_grid_22, ai_governance_legitimacy__democratic_pluralist_reading, stakes_inflation(organizational), 25, 0.47).
narrative_ontology:measurement(ai_g_grid_23, ai_governance_legitimacy__democratic_pluralist_reading, stakes_inflation(structural), 0, 0.39).
narrative_ontology:measurement(ai_g_grid_24, ai_governance_legitimacy__democratic_pluralist_reading, stakes_inflation(structural), 25, 0.43).
narrative_ontology:measurement(ai_g_grid_25, ai_governance_legitimacy__democratic_pluralist_reading, suppression(class), 0, 0.42).
narrative_ontology:measurement(ai_g_grid_26, ai_governance_legitimacy__democratic_pluralist_reading, suppression(class), 25, 0.48).
narrative_ontology:measurement(ai_g_grid_27, ai_governance_legitimacy__democratic_pluralist_reading, suppression(individual), 0, 0.18).
narrative_ontology:measurement(ai_g_grid_28, ai_governance_legitimacy__democratic_pluralist_reading, suppression(individual), 25, 0.24).
narrative_ontology:measurement(ai_g_grid_29, ai_governance_legitimacy__democratic_pluralist_reading, suppression(organizational), 0, 0.28).
narrative_ontology:measurement(ai_g_grid_30, ai_governance_legitimacy__democratic_pluralist_reading, suppression(organizational), 25, 0.35).
narrative_ontology:measurement(ai_g_grid_31, ai_governance_legitimacy__democratic_pluralist_reading, suppression(structural), 0, 0.22).
narrative_ontology:measurement(ai_g_grid_32, ai_governance_legitimacy__democratic_pluralist_reading, suppression(structural), 25, 0.26).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__democratic_pluralist_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__market_libertarian_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__technocratic_optimization_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the ai_governance_legitimacy kernel. It is linked to three sibling constraints instantiating alternative readings: magisterial_subsidiarity_reading (ecclesiastical authority), market_libertarian_reading (market discipline), technocratic_optimization_reading (expert authority). These are not variants or perspectives of one constraint; they are four distinct constraints with different epsilon values, beneficiary structures, and classifications, unified only by the contested kernel text they interpret. Each reading should be authored as a clean, epsilon-invariant constraint; network.affects_constraints records the sibling relationships. The epsilon differential between readings reflects how different interpretive frameworks define 'AI governance legitimacy' differently—magisterial reading sees moderate extraction (0.40–0.50) as enforcement of binding principles; democratic-pluralist reading sees it as coordination cost; technocratic reading sees it as constraint on optimization. The kernel is the ambiguous or fixed text (encyclical, governance principles) all readings cite; the readings are the competing interpretations of what that text legitimates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
