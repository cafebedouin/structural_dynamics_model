% ============================================================================
% CONSTRAINT STORY: horizon_liability_contract
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_horizon_liability_contract, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: horizon_liability_contract
 *   human_readable: Post Office Horizon Contractual Liability
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   The Post Office Horizon system scandal represents one of the most severe
 *   contractual extraction mechanisms in modern UK institutional history.
 *   From 1999 onwards, the Post Office required sub-postmasters — independent
 *   contractors operating ~11,500 local branches — to use the faulty Horizon
 *   IT system and personally indemnify the corporation for any financial
 *   shortfalls the system reported. When Horizon's accounting became
 *   unreliable (due to software defects, data corruption, and synchronization
 *   failures), sub-postmasters were held personally liable: they faced
 *   criminal prosecution, civil recovery actions, imprisonment, and loss of
 *   livelihood for shortfalls they did not create. The constraint operated
 *   through contractual obligation and suppressed alternatives:
 *   sub-postmasters could not opt for alternative systems, could not exit the
 *   contract without license revocation, and faced institutional denial of
 *   system defects from Post Office management and Fujitsu. The suppression
 *   mechanism held for over 20 years, with Post Office audits systematically
 *   misrepresenting Horizon reliability, whistleblowers ignored, and the
 *   institution defending itself against 555+ legal cases brought by victims.
 *   The constraint exhibits all hallmarks of a Snare: high extraction (forced
 *   personal liability for institutional failures), high suppression
 *   (contractual obligation, no alternatives, institutional denial), forced
 *   labor of the verification function (sub-postmasters had to debug and
 *   compensate for system failures at personal cost), and a clear asymmetry
 *   between beneficiary (Post Office Corporation and Fujitsu, protected from
 *   accountability) and victim (sub-postmasters, bearing full cost). The
 *   theater ratio reflects the contractual legitimacy framing: the constraint
 *   was presented as normal commercial risk allocation (contractors assume
 *   operational risk), obscuring its true function as systematic liability
 *   transfer from sophisticated institutions to atomized individuals.
 *
 * KEY AGENTS:
 *   - Sub-Postmasters (UK): Primary victims (powerless/trapped) — 555+ prosecuted; forced personal liability for system defects; no exit option without losing license and livelihood
 *   - Post Office Corporation: Primary beneficiary (institutional/arbitrage) — shifts accountability to contractors; protected from system liability; maintains branch network without capital investment in reliable systems
 *   - Fujitsu Systems: Secondary beneficiary (institutional/arbitrage) — contracted to build and maintain Horizon; protected from defect liability by user attribution; revenue from contracts continues despite system failures
 *   - Postal Service Institutional Integrity: Primary victim (powerless/trapped) — false liability claims corrupt financial audit trail; public trust undermined; institution compelled to defend unjust prosecutions
 *   - Sub-Postmaster Coalition (post-2009) / Public Inquiry (2020+): Organized agents (organized/constrained) — discovered suppression mechanism and extracted confessions; driving legislative reform and compensation; constrained by institutional inertia and legal complexity
 *   - Analytical Observer: Civilizational transparency view (analytical/analytical) — recognizes the canonical Snare structure: base extraction + suppression + no exit = coercive extraction masked by contractual legitimacy theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(horizon_liability_contract, 0.78).
domain_priors:suppression_score(horizon_liability_contract, 0.82).
domain_priors:theater_ratio(horizon_liability_contract, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(horizon_liability_contract, extractiveness, 0.78).
narrative_ontology:constraint_metric(horizon_liability_contract, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(horizon_liability_contract, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(horizon_liability_contract, snare).
narrative_ontology:human_readable(horizon_liability_contract, "Post Office Horizon Contractual Liability").
narrative_ontology:topic_domain(horizon_liability_contract, "economic/technological/legal").

domain_priors:requires_active_enforcement(horizon_liability_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(horizon_liability_contract, post_office_corporation).
narrative_ontology:constraint_beneficiary(horizon_liability_contract, fujitsu_systems).
narrative_ontology:constraint_victim(horizon_liability_contract, sub_postmasters).
narrative_ontology:constraint_victim(horizon_liability_contract, postal_service_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUB-POSTMASTER (SNARE) — Contractually required to use Horizon system, personally liable for shortfalls even when system fails. No exit: breach of contract triggers license revocation, loss of livelihood, and legal action. Forced to either falsify accounts or pay deficits from personal funds. d≈0.98, f(d)≈1.48, σ=1.0 → χ≈1.15. Maximum extraction under coercion.
constraint_indexing:constraint_classification(horizon_liability_contract, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POSTAL SERVICE INTEGRITY (SNARE) — The constraint undermines the epistemic foundation of postal accounting. False liability claims contaminate the institution's financial records and breach public trust. No exit option: the institution cannot opt out of its accounting obligations or audit liabilities. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈1.11. Extraction from institutional legitimacy.
constraint_indexing:constraint_classification(horizon_liability_contract, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POST OFFICE CORPORATION (ROPE) — Contracts create apparent coordination: system accountability is shifted to sub-postmasters, reducing corporate liability exposure. Exit: Post Office can breach contracts, amend terms, or shift vendor. d≈0.08, f(d)≈-0.18, σ=1.0 → χ≈-0.14. Net beneficiary; experiences constraint as liability mitigation mechanism.
constraint_indexing:constraint_classification(horizon_liability_contract, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FUJITSU SYSTEMS (ROPE) — Contract terms protect vendor from accountability for system defects. Shortfalls are attributed to user error (sub-postmaster), not software failure. Exit: vendor can refuse to service or maintain system. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Net beneficiary; experiences constraint as liability shield.
constraint_indexing:constraint_classification(horizon_liability_contract, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SUB-POSTMASTER COALITION / PARLIAMENTARY INQUIRY (TANGLED ROPE) — Organized agents have identified the structural extraction and initiated formal remediation (public inquiry, convictions overturned, compensation). The constraint has a genuine coordination function (system standardization) but masked by extractive liability assignment. Constrained exit: reform requires legislative and institutional change, not individual contractor choice. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.37. Moderate effective extraction because organization and transparency have begun reducing suppression.
constraint_indexing:constraint_classification(horizon_liability_contract, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, this constraint exhibits the canonical Snare pattern: base extraction (0.78) × suppression (0.82) creates a high-coercion, low-exit-option mechanism. The theater ratio (0.68) reflects contractual legitimacy theater — the constraint is framed as normal commercial risk allocation, obscuring its extractive structure. d≈0.70, f(d)≈1.10, σ=1.0 → χ≈0.86. The constraint operates through informational and contractual asymmetry, not through physical force, but the coercive mechanism is structurally identical: trapped exit, liability transfer, suppression of alternatives.
constraint_indexing:constraint_classification(horizon_liability_contract, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(horizon_liability_contract_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(horizon_liability_contract, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(horizon_liability_contract, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(horizon_liability_contract, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(horizon_liability_contract_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The constraint directly transfers institutional liability to atomized contractors. Sub-postmasters faced: criminal prosecution for amounts they did not steal, civil recovery for accounting defects they did not create, license revocation threatening their livelihood, and personal bankruptcy. The extraction escalated over the interval as the system's unreliability accumulated and Post Office doubled down on enforcement (the early 2000s saw aggressive prosecutions despite growing technical evidence of Horizon defects). The 0.78 value reflects the magnitude of personal liability transfers — some sub-postmasters lost entire life savings and spent years imprisoned. Suppression (0.82): Very high. Multiple suppression mechanisms locked the constraint in place: (1) Contractual obligation — sub-postmasters could not refuse Horizon without license revocation; (2) Institutional denial — Post Office management, auditors, and executives systematically misrepresented system reliability; (3) Expertise asymmetry — sub-postmasters lacked technical expertise to diagnose Horizon defects; (4) Atomization — individual contractors could not collectively challenge the Post Office; (5) Legal capture — prosecution infrastructure was turned against victims (sub-postmasters prosecuted for crimes committed by the system). Theater ratio (0.68): Moderate-high. The constraint is legitimized through contractual theater: the arrangement is presented as standard commercial risk allocation (contractor assumes operational risk), which obscures its extractive function. Contracts appear neutral — both parties 'agreed' — but the asymmetry (Post Office is institutional monopoly; sub-postmasters are dependent contractors) means the agreement is not freely negotiated. The theater increased over the interval as the Post Office's litigation strategy became more aggressive despite mounting technical evidence of system failure.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The sub-postmaster sees a Snare: trapped in a contract, forced to pay for system defects, facing criminal prosecution and imprisonment. The Post Office sees a Rope: the contract coordinates system standardization and allocates operational risk. Fujitsu sees a Rope: the contract protects vendor liability. The postal institution sees a Snare: its financial audit trail is corrupted by false liability claims, and its legitimacy is damaged by defending unjust prosecutions. The organized coalition sees a Tangled Rope: the constraint has a genuine coordination function (system standardization) but executed through extractive liability assignment that active enforcement has exposed and begun to remedy. The analytical observer sees a canonical Snare: the constraint is a textbook example of how contractual legitimacy theater masks coercive extraction. The gap between beneficiary (who experience coordination) and victim (who experience pure extraction) is absolute.
 *
 * DIRECTIONALITY LOGIC:
 *   Sub-postmasters: Victim + trapped → d≈0.98, f(d)≈1.48. Maximum extraction. They cannot exit the contract without losing license; they are forced to pay personal liability for system defects they did not create. The trapped exit combined with victim status produces the highest directionality. Post Office Corporation: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.18. Net beneficiary. The corporation can exit at any time (amend contracts, replace Horizon, settle claims), and it benefits from shifted liability. Fujitsu: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary. Vendor has exit options (refuse contracts, retire product, cease maintenance) and benefits from liability protection. Postal Service Integrity: Victim + trapped → d≈0.95, f(d)≈1.42. Trapped in institutional obligation to audit and defend the constraint; cannot exit without institutional dissolution. Sub-Postmaster Coalition: Organized + constrained → d≈0.45, f(d)≈0.48. Constrained by legal and political complexity of reform, but organized agents have begun reducing suppression through institutional channels (inquiry, legislation, compensation schemes). Analytical Observer: d≈0.70, f(d)≈1.10. Neutral analytical stance; sees the constraint from outside the institutional dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint definitively resolves as a Snare. The mandatrophy question is: 'Does this constraint primarily coordinate an action (Rope/Tangled Rope) or primarily extract value (Snare/Piton)?' The evidence overwhelmingly establishes that the primary function is extraction disguised as coordination. The coordination function (system standardization) could have been achieved without personal liability transfer (vendors bear defect liability, pooled institutional risk, or third-party assurance). The choice of personal liability was an extractive mechanism, not a necessary coordination requirement. Evidence: (1) International comparables: other postal systems (Canada Post, Australia Post) deployed national IT systems without personal contractor liability; (2) Temporal analysis: extractiveness increased over the interval (0.45→0.78) despite system failures accumulating, indicating Post Office doubled down on extraction rather than fixing the coordination problem; (3) Suppression durability: the institution spent resources suppressing evidence of system defects (litigation, institutional denial) rather than resolving coordination problems (system repair, vendor accountability); (4) Asymmetry persistence: the beneficiary-victim gap remained absolute for 20+ years, with no renegotiation or burden-sharing despite accumulating evidence; (5) Organized opposition threshold: when victims achieved organizational power (coalition formation, public inquiry), the constraint was immediately exposed as unjust, indicating it had never achieved consensual legitimacy. The constraint is therefore a canonical Snare: base extraction (0.78), high suppression (0.82), effective extraction χ≈0.86, and sustained through coercion and institutional denial rather than through perceived legitimacy or mutual coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    system_defect_attribution,
    'How should shortfalls be attributed between software defect and user error when the system''s audit trail is compromised or manipulated?',
    'Forensic analysis of system logs, vendor source code review, comparison with independent audits, post-facto testing of historical transactions',
    'If primarily software defect: snare classification is correct; liability transfer is unjustified extraction. If mixed: tangled rope partially defensible as risk allocation. If primarily user error: constraint appears as legitimate coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(system_defect_attribution, empirical, 'Attribution of shortfalls between software defect and user error').

omega_variable(
    contractual_necessity_justification,
    'Was the personal liability requirement a necessary component of the coordination mechanism, or could standardized Horizon deployment have been achieved with vendor liability or pooled institutional risk?',
    'Counterfactual analysis: comparative study of international postal IT systems (Canada Post, Australia Post, Deutsche Post, Royal Mail), cost-benefit analysis of alternative liability models, vendor negotiation history',
    'If necessary: constraint is defensive coordination (Rope/Scaffold). If unnecessary: constraint is pure extraction disguised as coordination (Snare). This determines mandatrophy resolution rationale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractual_necessity_justification, empirical, 'Whether personal liability was necessary for system coordination').

omega_variable(
    suppression_mechanism_durability,
    'Why did the suppression mechanism (contractor silence, institutional denial, audit obstruction) persist for 20+ years despite growing discrepancies and whistleblower reports?',
    'Institutional analysis: Post Office governance structure, incentive alignment between management and branch operators, review of regulatory oversight failures, media silence period analysis',
    'If suppression was structural (auditing failures, contractual liability prevents collective action): snare classification confirmed. If suppression was incidental (management choice): constraint might be reformable as rope without fundamental restructuring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_durability, empirical, 'Why suppression mechanisms persisted despite evidence of system failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(horizon_liability_contract, 1999, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(horizon_tr_t1999, horizon_liability_contract, theater_ratio, 1999, 0.42).
narrative_ontology:measurement(horizon_tr_t2010, horizon_liability_contract, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(horizon_tr_t2020, horizon_liability_contract, theater_ratio, 2020, 0.68).

% Extraction over time
narrative_ontology:measurement(horizon_be_t1999, horizon_liability_contract, base_extractiveness, 1999, 0.45).
narrative_ontology:measurement(horizon_be_t2010, horizon_liability_contract, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(horizon_be_t2020, horizon_liability_contract, base_extractiveness, 2020, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(horizon_liability_contract, enforcement_mechanism).
narrative_ontology:affects_constraint(horizon_liability_contract, contractor_liability_transfer_mechanisms).
narrative_ontology:affects_constraint(horizon_liability_contract, institutional_audit_capture).

% DUAL FORMULATION NOTE:
% The Horizon scandal decomposes into two structurally distinct constraints: (1) horizon_liability_contract (ε=0.78, Snare) — the contractual mechanism of personal liability transfer; (2) post_office_institutional_denial (ε=0.65, Tangled Rope) — the institutional suppression mechanism that sustained the snare by denying system defects. The second constraint is downstream of the first: institutional denial was necessary to sustain extraction once system unreliability became apparent. They are linked as network dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(horizon_liability_contract, institutional, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
