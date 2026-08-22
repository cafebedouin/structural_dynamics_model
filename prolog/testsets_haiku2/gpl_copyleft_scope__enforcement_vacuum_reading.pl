% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope Under Enforcement Vacuum
 *   domain: intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL's copyleft clause (Section 2(b)) requires derivative works to be
 *   licensed under GPL. But the term 'derivative work' has never been
 *   judicially defined for modern software integration forms: dynamic
 *   linking, plugin architectures, cloud services, and distributed systems.
 *   Two readings coexist: the FSF-aligned strong-copyleft reading (copyleft
 *   extends to all forms of coupling) and the industry-pragmatist narrow
 *   reading (copyleft applies only to traditional source-level derivative
 *   works). No binding precedent exists. Judicial and legislative venues that
 *   could close this interpretive vacuum are effectively excluded from the
 *   governance structure. The absence of authoritative resolution becomes the
 *   constraint's defining feature: adopters must navigate licensed pluralism,
 *   bearing transaction costs and regulatory risk. This reading instantiates
 *   the constraint AS IT EXISTS — the uncertainty itself — rather than either
 *   of the sibling readings' clearer claims.
 *
 * KEY AGENTS:
 *   - FSF-aligned enforcement community: organizational agenda-setter, enforces strong-copyleft interpretation through community pressure and selective litigation support
 *   - Industry pragmatists: powerful payers, exploit narrow-scope reading to maintain proprietary extensions and derivative ecosystems
 *   - Clarity-seeking adopters: moderate-power payers, bear transaction costs of navigating unresolved scope
 *   - Plugin ecosystem developers: moderate-power payers, operate under narrow reading with residual enforcement risk
 *   - Large platform operators: institutional beneficiaries, navigate both readings simultaneously using scale and expertise
 *   - Individual FOSS contributors: powerless, identity-locked beneficiary-payers, value software freedom but cannot control enforcement of their copyrighted work
 *   - Courts and legislators: excluded institutional observers, could resolve the vacuum but do not intervene
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.42).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.31).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope Under Enforcement Vacuum").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '301b6a60-634a-4b9b-859c-be39d0092d96').
narrative_ontology:cs_kernel_codification('301b6a60-634a-4b9b-859c-be39d0092d96', fixed_text).
narrative_ontology:cs_authority_grounding('301b6a60-634a-4b9b-859c-be39d0092d96', distributed).
narrative_ontology:cs_reading_relation('301b6a60-634a-4b9b-859c-be39d0092d96', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('301b6a60-634a-4b9b-859c-be39d0092d96', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('301b6a60-634a-4b9b-859c-be39d0092d96', foundational, derivative_work_scope_is_indeterminate).
narrative_ontology:cs_axiom_status(derivative_work_scope_is_indeterminate, holdable).
narrative_ontology:cs_axiom_grounding('301b6a60-634a-4b9b-859c-be39d0092d96', derivative_work_scope_is_indeterminate, empirically_contingent).
narrative_ontology:cs_axiom('301b6a60-634a-4b9b-859c-be39d0092d96', secondary, interpretive_pluralism_licenses_decentralized_governance).
narrative_ontology:cs_axiom_status(interpretive_pluralism_licenses_decentralized_governance, holdable).
narrative_ontology:cs_axiom_grounding('301b6a60-634a-4b9b-859c-be39d0092d96', interpretive_pluralism_licenses_decentralized_governance, instrumental).
narrative_ontology:cs_reference_frame('301b6a60-634a-4b9b-859c-be39d0092d96', interpretive_pluralism_licensed).
narrative_ontology:cs_drift_state('301b6a60-634a-4b9b-859c-be39d0092d96', contemporary_cloud_and_microservices_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('301b6a60-634a-4b9b-859c-be39d0092d96', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_enforcement_community).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters_seeking_flexibility).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, plugin_ecosystem_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, plugin_ecosystem_developers).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, large_platform_operators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, individual_foss_contributors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, industry_pragmatists).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, individual_foss_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets GPL Section 2(b) maximally: copyleft extends to all derivative works including dynamic linking, plugin interfaces, and distributed systems. Enforces through community pressure, license review infrastructure, and selective litigation support. Benefits from maximalist interpretation because it advances software freedom ideology and forces adoption of GPL-licensed dependencies. Their enforcement capacity is organizational (FSF itself, Software Freedom Law Center) and community-based (license compliance monitoring networks).
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_enforcement_community, agenda_setter,
    organized, generational, constrained, global).

% Interprets GPL Section 2(b) narrowly: copyleft applies only to direct derivative works in traditional copyright sense; plugin architectures, dynamic linking, and aggregation are exempt. Their enforcement capacity is their market position and ability to absorb or reframe GPL obligations. They pay through uncertainty costs and occasional strategic licensing decisions, but exploit the vacuum to maintain proprietary extensions and derivative plugin ecosystems.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_pragmatists, payer,
    powerful, biographical, constrained, global).

% Want GPL scope to be clearly defined so they can design systems with confidence. The absence of definitive precedent creates transaction costs: they must commission legal analysis, implement conservative compliance strategies, and sometimes reject GPL integration entirely. They bear the cost of navigating between the two readings without authoritative resolution.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Develop third-party extensions and plugins for systems with GPL core codebases. The vacuum allows them to operate under the narrower reading (claiming exemption from copyleft via plugin interface isolation) but exposes them to enforcement risk from the FSF-aligned community. They benefit from the flexibility the vacuum allows, but pay through legal uncertainty and potential community sanctions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, plugin_ecosystem_developers, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, plugin_ecosystem_developers, beneficiary).

% Operate distributed systems and cloud platforms that incorporate GPL software. They exploit the enforcement vacuum by adopting narrow-reading compliance strategies: using GPL software in backend infrastructure while keeping proprietary services legally isolated. Their scale gives them resources to navigate both readings simultaneously and choose the interpretation advantageous in each context.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Contribute code to GPL projects and value software freedom. They benefit from the copyleft concept (ensures their work stays free) but pay through the enforcement vacuum: their copyrighted contributions are deployed in systems they cannot control, and the lack of clear scope means they cannot reliably assert copyleft intent. Identity-locked: they are committed to the FOSS mission and cannot exit the governance structure without betraying their values.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, individual_foss_contributors, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, individual_foss_contributors, payer).

% Have the capacity to close the enforcement vacuum through binding precedent or statutory clarification. Notably absent: no major jurisdiction has produced definitive case law on GPL copyleft scope in derivative works or plugin architecture contexts. Their exclusion is structural — FOSS licensing disputes are rarely litigated to judgment (settlements dominate, legal costs are prohibitive), and legislators have not intervened to clarify software derivative-work boundaries.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, courts_and_legislators, excluded,
    institutional, generational, analytical, national).

% Administers the GPL text and could issue authoritative guidance that would narrow the enforcement vacuum. Notably does not: FSF officially maintains that Section 2(b) is intentionally broad, but does not enforce maximally in practice, allowing interpretive space for the narrower reading. Acts as a referee rather than absolute judge, sustaining the vacuum.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_board_and_counsel, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared textual baseline (GPL Section 2(b)) that all software developers must navigate when combining code. The vacuum allows decentralized interpretation: each project, ecosystem, and community can apply the reading that fits its values, enforced by social and organizational pressure rather than courts. This permits FOSS governance to adapt without central authority.
% TRANSFER_FUNCTION: Moves legal risk and transaction costs from clear compliance pathways to adopters and contributors. FSF-aligned communities transfer their enforcement preferences to downstream users by establishing de facto copyleft norms; pragmatists transfer their narrow-reading advantages to proprietary extension ecosystems. Large platforms transfer their expertise advantage into the ability to navigate both readings simultaneously. Individual contributors transfer their copyrighted work into systems governed by interpretive pluralism they cannot control.
% ABSENT_VOICES: Courts and legislators are structurally excluded: their capacity to close the vacuum through binding precedent or statutory clarification would end the licensed plurality, and their absence permits the interpretive vacuum to persist. Open-source ethics communities advocating for universal strong copyleft and commercial actors seeking absolute legal clarity are both absent from authoritative resolution.
% DISAPPEARANCE_RATIONALE: If the enforcement vacuum closed — through binding precedent, statutory clarification, or FSF board action establishing authoritative scope — software ecosystems would reorganize. A strong-copyleft verdict would force proprietary extensions into separate legal entities and cloud service isolation (Amazon's approach requires GPL compliance rework). A narrow-scope verdict would accelerate plugin and modular architecture adoption and reduce GPL enforcement tension. The vacuum's absence would eliminate the transaction costs clarity-seekers currently bear and remove the strategic flexibility pragmatists exploit.
% FOUNDING_PROBLEM: GPL Section 2(b)'s language ('derivative work') was inherited from copyright law, which has never defined 'derivative' precisely for software combining forms (static linking, dynamic linking, plugin interfaces, distributed systems, cloud services). The FSF drafted GPL to maximize copyleft scope and preserve software freedom, but did not anticipate the technological forms (cloud, microservices, plugin ecosystems) that would later challenge whether coupling creates 'derivative works' in GPL terms.
% FOUNDING_PROBLEM_CORROBORATION: FSF's own licensing guidance (GPL-FAQ, later GPL-3.0 elaborations) attests that the founding problem exists and that the answer remains intentionally ambiguous. Pragmatic industry adopters and plugin ecosystem advocates attest that the lack of clarity is a problem they face daily. No corroboration from courts or academic legal consensus — the absence of external vindication is itself the corroboration: no authoritative outside seat has resolved the founding problem.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint's operation is fundamentally asymmetric: FSF-aligned communities extract through norm enforcement and interpretive authority, pragmatists extract through strategic interpretation flexibility, large platforms extract through expertise advantage. But extractiveness plateaus (stayed flat from 2015–2026) because the vacuum has reached equilibrium — no actor has achieved dominance that would drive further extraction increase. Suppression is lower (0.31) because the constraint does not foreclose exit via judicial relief or legislative action; pragmatists and large platforms can adopt narrow readings with calculated risk rather than full coercion. Theater ratio rises from 0.15 to 0.48 because enforcement has increasingly become performative: FSF-aligned communities issue compliance guidance and conduct license audits, but actual litigation is rare, and compliance is often staged-managed through community relations rather than applied as strict rule. The rising theater ratio over time reflects the hardening of community enforcement rituals without judicial enforcement power to back them. Accessibility collapse is high (0.65) because once you understand the GPL exists and that derivative work scope is contested, your technical options (how to link, how to distribute, what counts as coupling) are substantially constrained by the need to navigate both possible readings. Resistance is high (0.72) because pragmatists actively resist strong-copyleft interpretation through architectural choices, litigation defense, and reframing of technical coupling as non-derivative; individual contributors resist through repeated questioning of FSF guidance; courts implicitly resist by declining to litigate the scope. The measurement series traces the historical trajectory: low extractiveness in 1991 (GPL was new, few were subject to it) through 2026 (extractiveness stabilized once major platforms adopted pragmatist strategies). Theater ratio's rise reflects the growth of enforcement infrastructure without parallel growth in judicial or binding resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF-aligned community's seat, the constraint is coordination: a shared textual anchor (GPL Section 2(b)) that enables decentralized enforcement of software freedom principles. From the pragmatist and plugin-developer seats, the same constraint is asymmetric extraction: FSF-aligned communities impose interpretive burdens and legal risk without clear boundaries or authoritative consequences. Large platforms compute it as exploitable ambiguity (low directionality toward target, because their resources permit navigation of both readings). Individual contributors compute it as uncontrollable: they generate copyrighted work deployed under uncertain licensing terms. The engine should derive high directionality variance across seats from this structural asymmetry: agenda-setters near beneficiary end, payers near target end, large platforms near symmetric, individual contributors trapped by identity.
 *
 * DIRECTIONALITY LOGIC:
 *   FSF-aligned community (agenda_setter, institutional power, generational horizon): derives low d (near beneficiary) — sets the agenda, enforces through organizational capacity, extracts compliance through norm authority. Industry pragmatists (payer, powerful, biographical horizon): derives high d (near target) — constrained by GPL text and social pressure, bear transaction costs of dual-reading navigation, limited exit because GPL is already embedded in their dependencies. Clarity-seeking adopters (payer, moderate, biographical): very high d — bear transaction costs directly, cannot resolve the ambiguity through their own authority, stuck in the uncertainty. Plugin developers (payer + beneficiary, moderate, mobile exit): moderately high d — exploit flexibility of narrow reading (benefit) but exposed to enforcement risk (cost). Large platforms (beneficiary, institutional, arbitrage exit): low d — possess enough expertise and scale to navigate both readings, access to proprietary alternatives, genuine strategic flexibility. Individual contributors (beneficiary + payer, powerless, identity_locked): moderately high d — benefit from copyleft principle but cannot control how their work is licensed in composite systems. Courts/legislators (excluded, institutional, analytical): neutral analytical seat, no directionality (no structural relationship to extraction or benefit from the constraint's operation).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits Mandatrophy: the founding problem (unclear scope of derivative works under GPL) remains live and contested, but the constraint's persistence depends increasingly on its UNSOLVED status. If courts or FSF resolved the scope definitively, the constraint would transform: strong-copyleft resolution would make the constraint clearer, more enforceable, and more extractive; narrow-scope resolution would dissolve much of the FSF-aligned enforcement pressure. The equilibrium depends on the vacuum's continuation. FSF-aligned communities benefit from interpretive authority in the absence of judicial override; pragmatists benefit from the ambiguity permitting flexibility. Both seats have incentive to sustain the unresolved state. The mandate (GPL Section 2(b) shall govern derivative works) has outlived its original purpose (clear rule for combining GPL software) because the rule is no longer clear and the constraint persists as political stalemate rather than functional coordination. Theater ratio's rise (0.15 to 0.48) traces the replacement of rule with ritual: enforcement becomes performative (audit statements, guidance documents, occasional license reviews) rather than decisive (judicial remedies, binding interpretations). The constraint meets the piton pattern at the organizational level (FSF maintains enforcement infrastructure that could be disbanded without loss of function, because the vacuum would persist anyway), but stays tangled_rope at the adoption level (adopters genuinely need guidance on how to comply, and the FSF provides a real coordination service even if enforcement is theatrical).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_source,
    'Which interpretive community''s reading of GPL Section 2(b) will achieve institutional dominance: FSF-aligned maximalism, industry pragmatism, or will the vacuum persist indefinitely as a permanent feature of FOSS governance?',
    'Binding judicial precedent from a major jurisdiction (EU, US), authoritative statutory clarification via copyright law reform, or explicit FSF board action establishing a canonical reading and enforcing it through license-stewardship power.',
    'Strong-copyleft dominance would increase extractiveness to ~0.75, eliminate theater ratio rise (enforcement would become legally binding), and force proprietary ecosystems into architectural isolation or GPL adoption. Narrow-scope dominance would drop extractiveness to ~0.15, eliminate suppression entirely, and permit integration without copyleft friction. Vacuum persistence would continue current equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_authority_source, empirical, 'Whether the enforcement vacuum will be judicially closed or remain structurally open.').

omega_variable(
    derivative_work_boundary_in_software,
    'Is there a coherent definition of ''derivative work'' in software that distinguishes GPL-triggering coupling from non-triggering aggregation, and does that definition follow traditional copyright law or require new legal frameworks?',
    'Comparative legal analysis across jurisdictions that have litigated software licensing disputes (EU Copyright Directive cases, GPL litigation outcomes if any reach appellate judgment, empirical survey of how courts have treated software combination cases in other licensing contexts).',
    'If a coherent boundary exists and follows copyright law, the narrow reading wins de facto. If coupling-based definitions (dynamic linking as derivative, plugin architectures as separable) become accepted, pragmatism wins. If no coherent boundary exists, the vacuum is irresolvable without statutory innovation — forcing choice between clarity-at-cost and pluralism-at-uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_boundary_in_software, empirical, 'Whether software derivative-work scope can be legally resolved within copyright doctrine.').

omega_variable(
    enforcement_vacuum_as_feature,
    'Is the interpretive vacuum a defect in GPL design that should be closed, or is it a structurally functional feature that permits decentralized FOSS governance without central authority?',
    'Qualitative research on how projects and adopters actually navigate the ambiguity: do they experience it as costless flexibility (feature) or costly uncertainty (defect)? Do FOSS governance outcomes improve or degrade under pluralism vs. under a definitive reading?',
    'If the vacuum is a feature, attempts to close it would harm FOSS governance and should be resisted. If it is a defect, FSF and institutional actors have incentive to resolve it. This axiom determines whether silence from courts/legislators reflects neutral exclusion or strategic choice by FSF.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vacuum_as_feature, preference, 'Whether the enforcement vacuum serves or harms FOSS governance values.').

omega_variable(
    suppression_internalization_in_adopters,
    'Do adopters who navigate the GPL scope ambiguity under the narrow-reading interpretation experience suppression as structural (external legal risk from FSF enforcement) or as internalized (they have incorporated GPL risk management into their own design norms)?',
    'Post-resolution experiment: if precedent closes the vacuum toward narrow-scope, do pragmatist adopters change their technical practices, or have they already baked GPL-aware design into their systems? If practices do not change, suppression was internalized; if they do, it was structural.',
    'If internalized, pragmatists have absorbed copyleft norms into their own values and cannot easily exit even if legal pressure disappears — the constraint is more persistent than legal enforcement alone suggests. If structural, removing legal pressure would permit rapid technical and architectural reversion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_adopters, empirical, 'Whether suppression of proprietary design patterns is external or internalized in pragmatist ecosystems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 1991, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1991, 0.15).
narrative_ontology:measurement(gpl__tr_t2000, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(gpl__tr_t2008, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2008, 0.37).
narrative_ontology:measurement(gpl__tr_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(gpl__tr_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2020, 0.47).
narrative_ontology:measurement(gpl__tr_t2026, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1991, 0.25).
narrative_ontology:measurement(gpl__be_t2000, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(gpl__be_t2008, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement(gpl__be_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(gpl__be_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(gpl__be_t2026, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1991, 0.12).
narrative_ontology:measurement(gpl__su_t2000, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(gpl__su_t2008, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2008, 0.23).
narrative_ontology:measurement(gpl__su_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(gpl__su_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(gpl__su_t2026, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2026, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__enforcement_vacuum_reading, 0.18).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel gpl_copyleft_scope. The strong_copyleft_reading and narrow_scope_reading are sibling constraints under the same kernel. All three stories share the referent (GPL Section 2(b)'s scope) but author different ε values because they describe different readings of the same text. The enforcement_vacuum_reading describes the constraint as it exists while unresolved — the actual operating reality of licensed pluralism. The siblings describe what the constraint would be if resolved toward strong or narrow scope. The network edges represent structural dependence: enforced resolution of the kernel (toward strong or narrow scope) would eliminate the enforcement_vacuum_reading and instantiate one of the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, powerless, 0.58).
constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
