% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_secular_democratic, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: Charter Secular-Democratic Institutional Framework with Military Subordination
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   A post-revolutionary state adopted a charter mandating secular democratic
 *   institutions and military subordination to civilian authority. This
 *   constraint instantiates ONE READING of a contested kernel about how
 *   post-revolutionary sovereign authority should be organized. Three parties
 *   propose different readings: secular democratic movements frame the
 *   charter as establishing civilian democratic supremacy; military-custodian
 *   advocates argue the charter ratifies military as permanent institutional
 *   guardian; political Islam movements frame the charter as illegitimately
 *   foreclosing religious-nationalist legitimacy. The
 *   secular_democratic_reading (this constraint) describes the arrangement as
 *   seen from the secular democratic position: the charter creates a binding
 *   institutional framework excluding competing claims. Political Islam
 *   actors and military institutional interests view the same charter as an
 *   imposed constraint that forecloses their legitimate sovereign claims. The
 *   measurement divergence reflects this: extractiveness is high (0.68)
 *   because the constraint's operation requires active suppression of
 *   competing institutional claims; suppression is high (0.72) because
 *   military and religious actors continually resist; theater is moderate
 *   (0.42) because some enforcement activity defends real democratic
 *   coordination (legislative rights, equality before law) while some defends
 *   the exclusion itself (preventing religious or military institutional
 *   alternatives from taking power). The constraint is tangled_rope from the
 *   secular democratic reading's perspective: it coordinates secular actors
 *   around democratic legitimacy AND extracts from military and religious
 *   actors through institutional exclusion. From the excluded seats, it
 *   appears as snare. The engine computes per-seat classifications from the
 *   structural data; the authored claim reflects the secular reading's own
 *   position.
 *
 * KEY AGENTS:
 *   - secular_democratic_movements: Primary beneficiaries (constraint vindicates their program and excludes competitors). Power: organized. Constituency: urban professionals, secular intelligentsia, civil society. Exit: constrained by political organization requirements.
 *   - political_islam_organizations: Primary victims (excluded from institutional power, identity-locked to the ideology the constraint forecloses). Power: organized. Identity lock: organizational identity fused with religious sovereignty claim. Exit: would require ideological transformation.
 *   - military_institution: Secondary victim/beneficiary (loses autonomous authority but retains operational autonomy and state resources). Power: powerful. Constrained: formally subordinate but operationally autonomous. Resistance expressed through bureaucratic resistance and occasional coup attempts.
 *   - civilian_legislative_assembly: Agenda-setter (vests sovereignty in elected body, enforces secular democratic mandate). Power: institutional. Constraint: dependent on continuous secular-democratic electoral majorities to maintain mandate.
 *   - civil_society_actors: Beneficiaries (secular institutional framework protects freedoms). Power: moderate. Exit: mobile but constrained by organizational restrictions on excluded ideologies.
 *   - international_democratic_actors: Observers (provide legitimacy signals, aid conditionality, diplomatic pressure supporting secular reading). Power: institutional (external). Exit: analytical.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.72).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "Charter Secular-Democratic Institutional Framework with Military Subordination").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, '08f2da62-06f7-4b29-a2e9-fba58bcf2dc2').
narrative_ontology:cs_kernel_codification('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', formalized).
narrative_ontology:cs_authority_grounding('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', lineage).
narrative_ontology:cs_interpretation_layer_present('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2').
narrative_ontology:cs_reading_relation('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', foundational, democratic_sovereignty_secular_grounded).
narrative_ontology:cs_axiom_status(democratic_sovereignty_secular_grounded, holdable).
narrative_ontology:cs_axiom_grounding('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', democratic_sovereignty_secular_grounded, deontological).
narrative_ontology:cs_axiom('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', foundational, military_subordination_structural).
narrative_ontology:cs_axiom_status(military_subordination_structural, holdable).
narrative_ontology:cs_axiom_grounding('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', military_subordination_structural, deontological).
narrative_ontology:cs_axiom('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', secondary, secular_institutional_supremacy).
narrative_ontology:cs_axiom_status(secular_institutional_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', secular_institutional_supremacy, conventional).
narrative_ontology:cs_reference_frame('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', secular_democratic_sovereign_state).
narrative_ontology:cs_drift_state('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', contemporary_political_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('08f2da62-06f7-4b29-a2e9-fba58bcf2dc2', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_movements).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_actors).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_organizations).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institution_autonomous_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institution).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_movements).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The charter vests sovereign legislative authority in this body, which is mandated to operate on secular constitutional principles and to subordinate military institutions to civilian democratic governance. The assembly interprets and amends the charter; it sets the legal and institutional framework for all governance. Its power is formally supreme but depends on continuous enforcement against institutional resistance.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_legislative_assembly, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the charter's mandate for secular institutions and civilian democratic legitimacy — the constraint vindicates their political program and excludes competitors (religious nationalism movements) from institutional power. They must sustain political organization and mobilization to maintain charter-aligned legislative majorities; if they lose electoral power, the constraint becomes harder to enforce against institutional capture.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_movements, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_movements, payer).

% Pay the cost of constraint through exclusion or institutional subordination: forbidden from invoking religious law as sovereign legitimacy, barred from military institutional roles, restricted in advocacy of theocratic alternatives. Their organizational identity is fused with the ideological program the constraint forecloses (religious sovereignty). Exit would require ideological transformation. Resistance persists through shadow networks, electoral mobilization in pockets where constraints relax, and appeals to extra-institutional legitimacy (religious authority parallel to the state).
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_organizations, payer,
    organized, generational, identity_locked, national).

% Bears the constraint through formal subordination to civilian legislative authority and loss of autonomous governance prerogative. In practice retains substantial institutional autonomy (budget authority, operational independence, internal promotion rules) but formally cannot initiate policy or claim independent sovereignty legitimacy. The constraint extracts autonomy symbolically and intermittently (legislative review, civilian defense minister) while leaving operational power intact. Institutional resistance is expressed through bureaucratic slowness, intelligence asymmetries, and occasional coup attempts. Military also benefits from the constraint's stability — it receives state resources and retains operational authority within the subordinated framework.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institution, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institution, beneficiary).

% Benefit from the secular institutional framework through freedoms of expression, assembly, and conscience that the charter protects against both theocratic and military claims. Can organize independently of religious authority and military command. Their mobility is constrained by the charter's enforcement apparatus (surveillance, restrictions on organizing around excluded ideologies), but exit options remain open through relocation, alternative organizing, or quiet non-participation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_actors, beneficiary,
    moderate, biographical, mobile, national).

% External stakeholders (foreign governments, international democracy organizations) who endorse the secular democratic reading and provide legitimacy signals, diplomatic recognition, and sometimes material support to the civilian institutions. They observe compliance and shape incentive structures through aid conditionality and diplomatic pressure; they remain external to the constraint's immediate enforcement but amplify its stability through recognition and sanction.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_actors, observer,
    institutional, generational, analytical, continental).

% Would argue for institutional design where military retains autonomous guardian authority and charter revision to permit military policymaking independence. Structurally excluded from authoritative voice in charter interpretation by the secular democratic reading itself; their position is that military institutionalism (not religious legitimacy and not pure civilian supremacy) is the true sovereign foundation. Inclusion in decision-making about charter design would immediately contest the constraint.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_custodian_advocates, excluded,
    organized, generational, identity_locked, national).

% Extra-institutional religious authorities (clerical councils, fatwa bodies, religious courts) would claim competing sovereignty claims grounded in divine law rather than democratic secular constitution. The charter excludes this institutional layer from state authority (though not from parallel social authority). These actors are structurally barred from charter interpretation; their voice would immediately reframe the entire sovereign question.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_authority_parallel_structures, excluded,
    organized, civilizational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_movements).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified secular democratic institutional framework for sovereign authority: removes competing legitimacy claims (religious theocracy, military guardianship) and creates a single source of law (democratic legislation interpreted through secular constitutional principles). Solves the multi-source-authority problem that plagued early post-revolutionary instability.
% TRANSFER_FUNCTION: Transfers institutional autonomy FROM military and religious authority structures TO civilian legislative authority. Moves ideological sovereignty FROM religious-nationalist framing TO secular-democratic framing. Both transfers are enforced through legal restriction, institutional design, and suppression of competing authority claims.
% ABSENT_VOICES: Political Islam organizations and military custodian advocates are structurally excluded from charter interpretation — their objections would directly contest the secular democratic reading itself. Religious parallel authorities remain outside the state framework entirely. The constraint's design excludes voices that would re-inscribe the sovereign question.
% DISAPPEARANCE_RATIONALE: If the charter's secular-democratic mandate vanished overnight, immediate institutional contests would erupt: military would claim governing prerogative based on stability arguments, political Islam organizations would re-mobilize for theocratic institutional design, and the state's legitimacy foundation would revert to a multi-source contest. The institutional consolidation would unravel within weeks as actors repositioned around competing sovereign claims.
% FOUNDING_PROBLEM: Post-revolutionary state faced three competing sovereignty claims with no institutional resolution: secular nationalist elites claimed sovereign democratic right, military institution claimed guardian prerogative for stability, and religious-nationalist movements claimed Islamic law as supreme legitimacy. Institutional paralysis and violent contestation ensued. The charter was drafted to impose a single legitimacy framework (secular democratic) on all three, subordinating the other two claims to it.
% FOUNDING_PROBLEM_CORROBORATION: Secular democratic movements and international observers attest the founding problem is live — military and religious actors continually challenge the charter's secular authority, requiring active enforcement. Military and political Islam actors attest the problem was falsely framed — that the charter imposed one faction's reading rather than resolving the genuine multi-source question. Independent historians document that post-revolutionary violence did in fact stem from unresolved sovereignty contestation; no independent external source corroborates the secular reading as the uniquely legitimate resolution (the corroboration comes from other secular reading proponents, not from outside the beneficiary coalition).
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is measured as tangled_rope because it combines real coordination (establishing secular democratic institutions that solve multi-source-authority chaos) with asymmetric extraction (military and religious actors are subordinated institutional competitors). Extractiveness rises over the interval (0.42 → 0.68) as the secular democratic reading consolidates institutional control and political Islam resistance hardens, requiring more active enforcement. Suppression rises correspondingly (0.48 → 0.72) because military and religious actors continually test the boundary — suppression is not a static institutional feature but an active enforcement requirement. Theater ratio rises moderately (0.18 → 0.42) because enforcement activity increasingly focuses on preventing institutional alternatives (blocking military coups, restricting religious party formation) rather than on substantive democratic governance. The measurement grid is shared across all three metrics at every time point to preserve temporal coherence and prevent the projection of end-state values into earlier periods. The measurements are marked 'observed' because the interval represents a documented historical trajectory where democratic consolidation, military subordination, and religious-organizational restriction are empirically attested developments.
 *
 * PERSPECTIVAL GAP:
 *   The civilian legislative assembly seat and the political Islam victims seat should compute fundamentally different constraint types. From the assembly's position, the constraint solves a coordination problem (multi-source authority chaos) through institutional consolidation and coordination around democratic legitimacy — it appears as rope or tangled_rope with net coordination benefit. From political Islam organizations' position, the same constraint is exclusionary extraction — it forecloses their institutional representation and identity-fusion claims through suppression, appearing as snare. The military seat sits between: the constraint extracts formal autonomy (theater of subordination) while leaving operational autonomy largely intact, making it appear as piton (degraded function, maintained theatrically). The engine's per-seat computation captures these divergences by computing directionality from power + exit + beneficiary/victim declarations separately for each seat, generating different classifications. This is correct; the constraint is genuinely experienced as different institutional arrangements depending on the structural position from which it is evaluated.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is structurally asymmetric across stakeholders. Secular democratic movements have d near 0.2 (beneficiaries: constraint subsidizes their legitimacy, removes competitors, requires no identity transformation from them). Political Islam organizations have d near 0.9 (targets: constraint extracts organizational autonomy, forecloses identity-fusion claims, requires exit via ideological transformation impossible for organizational identity). Military institution has d near 0.65 (partly target, partly beneficiary: loses autonomous policymaking authority but retains operational autonomy and state resources; identity remains organizationally intact but formally subordinated). Civil society actors have d near 0.35 (symmetric with modest extraction: gain secular freedom protections but carry diffuse enforcement costs and organizational restrictions on excluded-ideology advocacy). The directionality asymmetry across power atoms is the seat-divergence engine: institutional seats (civilian legislature, military, international observers) experience the constraint differently than organized movement seats (secular democratic, political Islam) because power positions the relationship to institutional change differently. The military's d is high despite retained operational autonomy because the constraint's core function is to formally subordinate military policymaking authority — that subordination is real even if operationally imperfect.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT show mandatrophy in the classical sense (mandate has not outlived its founding function). The founding problem was multi-source sovereign-authority contestation in post-revolutionary state. The charter's secular democratic mandate continues to address this problem by imposing a single legitimacy framework. However, the omega variable on 'kernel_reading_contestation_status' identifies a near-mandatrophy condition: if the constraint's holding power depends on contingent political dominance of secular movements rather than structural institutional self-protection, then a future electoral reversal could hollow it out. The measurement series shows extractiveness rising and plateauing rather than declining, which rules out the classical mandatrophy pattern (theater rising as function atrophies). Instead, the pattern suggests consolidation of the secular reading through hardening exclusion — extractiveness stabilizes because the reading has achieved institutional dominance and no longer needs to fight as hard to maintain it; suppression stabilizes because the excluded actors have been institutionally contained. This is not mandatrophy but it is a transition from contested kernel (three-way struggle) to consolidated hegemonic reading — the constraint's future is now hostage to political reversals rather than to functional atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_mandate_vs_religious_state_practice,
    'Does the charter''s secular institutional mandate reflect a genuine institutional settlement, or does it function as cover for ongoing parallel religious authority structures that retain de facto power over substantial domains (family law, religious endowments, cultural policy)?',
    'Empirical audit of decision-making authority: which institutional seat actually controls outcomes in domains nominally governed by secular law but historically administered by religious authority (marriage, inheritance, education curricula)? If secular legislative authority makes binding decisions, the mandate is real; if religious parallel structures make binding decisions with legal post-hoc ratification, the mandate is theatrical.',
    'If the mandate is theatrical (parallel authority structures retained despite secular institutional design), the constraint''s effective extractiveness is higher — it extracts legitimacy from the religious sphere while leaving power distributed, creating a hidden-coordination problem. The theater_ratio would be understated. If the mandate is real, extractiveness is accurately measured as institutional displacement of religious authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_mandate_vs_religious_state_practice, empirical, 'Whether secular institutional mandate reflects actual decision-making authority or covers retained parallel religious power.').

omega_variable(
    military_subordination_formal_vs_operational,
    'Is military subordination to civilian authority a structural institutional constraint on military policymaking, or is it a symbolic restraint that leaves operational autonomy intact while extracting formal obedience?',
    'Institutional audit over time: do civilian legislators set military policy (force structure, budget allocation, strategic doctrine) or does the military propose and the civilian authority formally ratify? Do coup attempts or institutional resistance by the military successfully alter civilian decisions, or do they fail? Does the military honor tenure limitations and civilian civilian-appointed leadership, or do senior officers resist and sometimes ignore civilian orders?',
    'If civilian authority structurally sets military policy, the constraint is a real institutional reallocation of power and the extraction is asymmetric (military pays in autonomy). If military retains operational autonomy and civilians provide legal cover, the constraint is primarily extractive (formal subordination without real power transfer) and theater_ratio is understated. This affects whether the constraint is tangled_rope (real coordination + asymmetric extraction) or snare (pure extraction disguised as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_subordination_formal_vs_operational, empirical, 'Whether military subordination is structural or theatrical.').

omega_variable(
    secular_democratic_reading_contested_kernel_identity,
    'Is the secular democratic reading a stable, self-sustaining institutional framework, or is it contingent on the demographic and political dominance of secular-oriented actors, such that it would transform if political Islam movements gained electoral majorities?',
    'Test case: if political Islam actors won electoral majorities in a genuinely contested election, what would happen to the charter''s secular mandate? Would the framework constrain them to secular legislative methods, or would they transform the framework itself? Historical precedents in constitutional democracies (Weimar, Indian secular constitution) offer comparative data on whether secular democratic frameworks are self-protective or party-contestable.',
    'If the secular framework is self-protective (structural constraints that cannot be overridden by any electoral majority), it is a genuine constitutional settlement. If it is party-contestable (can be amended by future majorities), the constraint is contingent on continued secular political dominance, and the security of political Islam organizations'' excluded status depends on electoral outcomes, not institutional structure. This affects the classification: if the constraint is self-protective, it is an institutional snare (permanent exclusion); if it is contingent, it is tangled_rope (coordination for the dominant coalition, extraction for the excluded, but contestable over time).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_democratic_reading_contested_kernel_identity, conceptual, 'Whether secular democratic reading instantiates a self-protective constitutional framework or a contingent political settlement.').

omega_variable(
    civilian_control_enforcement_capacity,
    'What concrete enforcement mechanisms exist to ensure military subordination to civilian authority? Are they institutional (courts, impeachment, legislative budget control) or dependent on active political will and military compliance?',
    'Institutional audit: identify specific enforcement mechanisms (constitutional courts, legislative oversight committees, budget conditionality, appointment/removal authority). Test their independence: do they function when the sitting executive or military resists? Do enforcement mechanisms survive political transitions? If enforcement depends on political will rather than institutional structure, the constraint is more fragile and theater_ratio understates the active suppression required to maintain it.',
    'Weak enforcement mechanisms (dependent on political will) mean the constraint is more extractive (requires continuous suppression) and more fragile (reversible if political balance shifts). Strong institutional mechanisms mean the constraint is a more stable institutional reallocation. The measurement of suppression_requirement would be higher if enforcement depends on contingent political support rather than structural institutional design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_control_enforcement_capacity, empirical, 'Strength and independence of institutional enforcement mechanisms for military subordination.').

omega_variable(
    kernel_reading_contestation_status,
    'This constraint is ONE READING of a contested kernel (july_charter_sovereign_legitimacy). The three sibling readings (guided_nationalism, military_custodian, secular_democratic) are held by different political coalitions. Is this a stable three-way contestation, or does one reading tend to dominate over time, rendering the others dormant?',
    'Historical trajectory: which reading has held institutional authority at different time periods? Do institutional shifts between readings happen at predictable junctures (electoral cycles, succession crises, coup attempts)? Do the sibling readings retain active constituencies and institutional capacity to challenge the dominant reading, or have they been subordinated into marginal positions?',
    'If one reading stably dominates, the constraint is as measured — a tangled_rope under the secular_democratic reading. If readings cycle or remain in active contestation, the constraint''s type is unstable and temporal measurements should show oscillations in extractiveness and suppression. The cyclical pattern would indicate that the constraint is held in place by contingent political balance rather than structural institutional settlement, raising the possibility of reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_status, empirical, 'Stability of the secular_democratic reading against sibling readings of the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading constraint family instantiating different interpretations of the post-revolutionary charter kernel. The secular_democratic_reading (this file) forecloses religious sovereignty claims and subordinates military authority. The guided_nationalism_reading instantiates the same kernel as establishing Islamic-nationalist legitimacy, with political Islam as beneficiary and secular movements as victims. The military_custodian_reading instantiates the kernel as ratifying military guardian authority, with both secular and religious actors subordinated. These are three structurally distinct constraints with different ε values, victim sets, and classifications. They are linked by network edges reflecting that institutional dominance of one reading constrains the feasibility of the others: if the secular_democratic reading achieves stable institutional dominance, the other readings are institutionally contained but remain ideologically live and can resurface at succession crises or military coups. The three readings remain in active contestation; the constraint's future is hostage to political reversals rather than to functional obsolescence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
