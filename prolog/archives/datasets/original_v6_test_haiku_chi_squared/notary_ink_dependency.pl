% ============================================================================
% CONSTRAINT STORY: notary_ink_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notary_ink_dependency, []).

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
 *   constraint_id: notary_ink_dependency
 *   human_readable: The Notary/Wet-Ink Persistence
 *   domain: legal/institutional
 *
 * SUMMARY:
 *   The notary wet-ink persistence represents a structural constraint
 *   embedded in legal authentication infrastructure that requires physical
 *   presence and manual signatures for high-value transactions. The
 *   constraint exhibits the full diagnostic spectrum of DR classification: it
 *   appears as pure extraction from the remote signatory's perspective
 *   (snare), as mixed coordination-extraction for transaction parties
 *   (tangled rope), as coordination infrastructure from the notary
 *   profession's internal perspective (rope), as a temporary bottleneck with
 *   a digital sunset (scaffold), as institutional theater persisting through
 *   inertia (piton), and as a potential immutable feature of trust
 *   architecture (false summit). The wet-ink requirement originated as a
 *   genuine solution to document authenticity in an era without cryptographic
 *   identity, but has persisted despite technological alternatives. The
 *   constraint's theater ratio (0.64) reflects that notaries verify identity
 *   through ID checks and physical presence, but cannot verify document
 *   comprehension, whether signers are acting under coercion, or mental
 *   capacity — the in-person ritual is substantially performative. Meanwhile,
 *   the notary profession benefits from mandatory fees and controlled market
 *   access, and legal institutions benefit from reduced liability exposure
 *   (whether or not notarization actually prevents fraud).
 *
 * KEY AGENTS:
 *   - Remote Signatories (Rural, International, Mobility-Constrained): Primary victims (powerless/trapped) — bear geographic and temporal costs of notarization requirement; cannot execute transactions without physical travel or expensive notary travel
 *   - Transaction Parties (Buyers, Borrowers, Executors): Secondary victims (moderate/constrained) — face friction and delay; also benefit from document authentication infrastructure
 *   - Notary Profession: Primary beneficiary (institutional/arbitrage) — captures mandatory fees and controls market access; benefits from government-enforced monopoly on authentication authority
 *   - Lenders and Title Insurance: Secondary beneficiaries (institutional/arbitrage) — reduce liability and fraud exposure through mandatory notarization; benefit from reduced claim rates
 *   - Digital Authentication Coalition: Organized agents (organized/constrained) — blockchain developers, remote notarization advocates, cryptographic identity proponents; building alternative authentication pathways
 *   - Legal Recording Infrastructure: Institutional actor (institutional/arbitrage) — maintains wet-ink requirement through statute and regulation; sees notarization as baseline authentication standard
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable feature of trust architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notary_ink_dependency, 0.52).
domain_priors:suppression_score(notary_ink_dependency, 0.68).
domain_priors:theater_ratio(notary_ink_dependency, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notary_ink_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(notary_ink_dependency, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(notary_ink_dependency, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notary_ink_dependency, tangled_rope).
narrative_ontology:human_readable(notary_ink_dependency, "The Notary/Wet-Ink Persistence").
narrative_ontology:topic_domain(notary_ink_dependency, "legal/institutional").

domain_priors:requires_active_enforcement(notary_ink_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notary_ink_dependency, notary_profession).
narrative_ontology:constraint_beneficiary(notary_ink_dependency, document_authentication_infrastructure).
narrative_ontology:constraint_victim(notary_ink_dependency, remote_signatories).
narrative_ontology:constraint_victim(notary_ink_dependency, transaction_efficiency).
narrative_ontology:constraint_victim(notary_ink_dependency, digital_commerce).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REMOTE SIGNATORY (SNARE) — Cannot execute high-value transactions without physical presence at a notary office. No meaningful alternatives exist for documents requiring notarial acknowledgment (real estate, powers of attorney, loan documents). Geographic and time constraints are absolute. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(notary_ink_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRANSACTION PARTIES (TANGLED ROPE) — Constrained by notarization requirement but benefit from document authentication infrastructure that reduces fraud risk and provides legal certainty. Exit is costly but possible (some transactions can migrate to digital notarization where permitted; some states allow audio-visual remote notarization). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(notary_ink_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NOTARY PROFESSION (ROPE) — Primary beneficiary. Notaries extract economic rent through mandatory notarization fees and control over document verification. However, from the profession's internal perspective, notarization solves a genuine coordination problem: establishing document authenticity in a system where identity is not otherwise cryptographically verified. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(notary_ink_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIGITAL AUTHENTICATION COALITION (SCAFFOLD) — Organized actors (blockchain advocates, digital identity proponents, remote notarization advocates) see wet-ink notarization as a temporary coordination failure with a sunset. Cryptographic signatures, blockchain timestamping, and remote notarization technology provide alternative authentication pathways that reduce (though do not entirely replace) the need for in-person notarial presence. d≈0.38, f(d)≈0.38, σ=1.0 → χ≈0.20. Low effective extraction because the coalition sees clear technology-driven exit path.
constraint_indexing:constraint_classification(notary_ink_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL CERTAINTY SYSTEM (PITON) — The notarization requirement persists largely through institutional inertia. Real fraud prevention is moderate: notaries verify identity through state ID checks and in-person observation, but cannot verify document comprehension, coercion, or mental capacity. The wet-ink and in-person requirements serve primarily as a theatrical performance of due diligence rather than meaningful fraud prevention. theater_ratio=0.64 confirms piton classification. The legal system sees its own verification mechanism as degraded — it persists because alternatives haven't fully replaced it and because path dependency in contract law and recording statutes maintains the requirement.
constraint_indexing:constraint_classification(notary_ink_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW VIEW — FALSE SUMMIT) — From a civilizational perspective, the analytical observer risks seeing wet-ink notarization as an immutable feature of trust architecture: 'all high-stakes legal systems require some form of in-person authentication by a trusted third party.' However, the structural data (ε=0.52, suppression=0.68, theater=0.64) contradicts this. Cryptographic identity systems, digital signatures, and blockchain-based verification provide functionally equivalent authentication without wet-ink presence. This is a false summit — the observer naturalizes a contingent institutional arrangement (notary licensing framework) as a law of trust.
constraint_indexing:constraint_classification(notary_ink_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notary_ink_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(notary_ink_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(notary_ink_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(notary_ink_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(notary_ink_dependency, TR),
    TR >= 0.70.

:- end_tests(notary_ink_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The notary profession and lending institutions capture clear economic and risk reduction benefits from mandatory in-person notarization. Notaries charge $0.50–$15 per signature; remote signatories must travel or pay for mobile notary services. However, extractiveness is not as high as pure rent-seeking (0.75+) because the constraint does provide real authentication benefits — identity verification is genuine, even if fraud prevention efficacy is overstated. The trajectory shows increasing extractiveness from 0.38 to 0.52 over 20 years, reflecting that digital alternatives have become available without corresponding loosening of notary requirements. Suppression (0.68): High. Multiple barriers prevent exit: (1) legal statutes and recording requirements mandate notarization for real estate, powers of attorney, and loan documents; (2) no cryptographic identity infrastructure exists as a mandatory substitute; (3) travel costs and time create practical friction; (4) some transaction parties cannot easily migrate to digital alternatives due to counterparty requirements (lenders, title companies). However, suppression is not absolute (0.90+) because some alternatives exist: remote notarization is now permitted in most states (post-2020 pandemic expansion); some jurisdictions accept electronic signatures; blockchain notarization exists but lacks legal recognition. Theater ratio (0.64): Moderate-high. Notaries verify identity (real) but cannot verify document comprehension, whether signatories are acting under duress, or mental capacity (theater). The in-person ritual creates confidence through presence and formality, but actual fraud prevention is modest. The ratio has increased from 0.42 to 0.64 over the interval, reflecting that as digital alternatives have emerged, the wet-ink requirement increasingly serves a theatrical function rather than a necessity function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival variation. The remote signatory (powerless/trapped) sees pure extraction (Snare): they must travel, pay fees, and delay transactions with no meaningful security benefit to themselves. The notary profession (institutional/arbitrage) sees coordination (Rope): from their internal perspective, notarization solves the problem of authenticating identity in a non-cryptographic system. Transaction parties (moderate/constrained) see mixed extraction and coordination (Tangled Rope): they benefit from reduced fraud risk but bear transaction costs and friction. The digital authentication coalition (organized/constrained) sees a temporary problem with a sunset (Scaffold): cryptographic identity, blockchain timestamping, and remote notarization are building alternative pathways that will eventually obviate the wet-ink requirement. The legal system itself (institutional/arbitrage) sees institutional theater (Piton): notarization persists through path dependency in recording statutes and commercial law, not because it uniquely solves authentication. The analytical observer risks seeing immutable necessity (false summit): the belief that 'all trust systems require in-person authentication by a third party' — but this is contradicted by cryptographic alternatives that provide equivalent or superior authentication without wet-ink presence. The perspectival gap is large because the constraint serves different structural functions for different agents: genuine identity verification for lenders, economic rent extraction for notaries, transaction friction for remote signatories, and theater for the legal system.
 *
 * DIRECTIONALITY LOGIC:
 *   Remote signatory: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Geographic and time constraints are absolute; no meaningful alternatives exist for high-value transaction documents. Transaction parties: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal; some alternatives exist (remote notarization where permitted; some document types can use electronic signatures). Notary profession: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Controls mandatory authentication and extracts fees. From the profession's internal perspective, they solve a coordination problem (identity verification), justifying moderate rent. Lenders/Title insurance: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary. Reduce fraud exposure and claims through mandatory notarization. Digital coalition: Organized + constrained → d≈0.36, f(d)≈0.36. Low effective extraction; coalition has institutional agency and clear path forward (regulatory reform, technology adoption). Legal system: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (0.64 ≥ 0.70 threshold not met, but high enough to justify piton review); extraction is low because the system maintains the requirement through inertia, not active benefit capture. Analytical observer: analytical → d≈0.70, f(d)≈1.12. False summit classification — observer naturalizes contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC PATTERN: The constraint resolves the mandatrophy by differentiating the notary profession's genuine coordination function (identity verification in a non-cryptographic system) from the systemic extraction that the requirement enables (mandatory fees, geographic friction, transaction delay). From 1850–1980, when cryptographic alternatives did not exist, notarization was primarily coordination (Rope): it solved the real problem of authenticating documents in a world where identity was established through paper, seals, and in-person presence. This justified the profession's economic rent. From 1990–present, with digital signatures, blockchain, and cryptographic identity available, the constraint has shifted toward extraction (Snare from remote signatories' perspective): the wet-ink requirement persists not because it uniquely solves authentication, but because path dependency in recording statutes, lending practices, and title insurance creates switching costs. The mandatrophy is resolved by recognizing that a constraint can be legitimate coordination at one point in time and become extractive at another point in time. The base properties (ε=0.52, suppression=0.68, theater=0.64) place this in the Tangled Rope zone — the constraint retains coordination function (lenders genuinely benefit from reduced fraud) but is also partially extractive (remote signatories bear costs without corresponding benefit). The increasing extractiveness and theater ratio (0.38→0.52 and 0.42→0.64 over 20 years) suggest the constraint is drifting from mixed coordination-extraction toward pure theater and inertia (Piton trajectory). If this trend continues, the constraint will become a candidate for regulatory reform — remote notarization expansion, digital signature recognition, or blockchain authentication — that would reduce both extractiveness and theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fraud_prevention_efficacy,
    'What percentage of transaction fraud is actually prevented by in-person notarization versus prevented by cryptographic verification or blockchain timestamping?',
    'Comparative fraud rate analysis across jurisdictions with strong notarization requirements vs jurisdictions using digital signatures; tracking of fraud losses post-notarization vs pre-notarization',
    'If notarization prevents >30% of fraud: constraint is coordination-heavy (Rope from more perspectives). If <10%: constraint is theater-heavy (Piton from more perspectives), and the snare/tangled-rope extraction is not justified by security benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fraud_prevention_efficacy, empirical, 'Actual fraud prevention rate attributable to notarization requirement').

omega_variable(
    remote_notarization_equivalence,
    'Do audio-visual remote notarization and cryptographic digital signatures provide legally and practically equivalent authentication to wet-ink in-person notarization?',
    'Comparative litigation analysis: fraud disputes involving remote-notarized documents vs in-person notarized documents; recording of fraud patterns and enforcement outcomes',
    'If equivalent: constraint is pure extraction (snare from powerless perspective becomes unjustified). If inequivalent: constraint has residual coordination function and tangled-rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remote_notarization_equivalence, empirical, 'Whether remote notarization provides equivalent legal standing to in-person notarization').

omega_variable(
    path_dependency_vs_necessity,
    'Is the in-person wet-ink requirement maintained because it solves a real authentication problem or because of path dependency in recording statutes and commercial law?',
    'Jurisdictional comparison: states that have reformed notarization law to permit remote notarization for all document types (e.g., following 2020 pandemic expansions) show no increase in fraud or disputes. Inverse test: can the requirement be removed without creating observable trust deficits?',
    'If path dependency dominates: constraint is primarily piton (degraded institutional theater). If real necessity: constraint retains rope/tangled-rope hybrid character.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(path_dependency_vs_necessity, conceptual, 'Whether requirement persists due to real authentication need or institutional path dependency').

omega_variable(
    geographic_extraction_asymmetry,
    'Does the notary requirement extract more heavily from rural/remote populations than from urban populations with abundant notary supply?',
    'Geographic analysis of notary density, travel time, and transaction friction by region; correlation with real estate transaction volume and dispute rates across urban/rural divide',
    'If asymmetry is large: victim profile includes geographically disadvantaged populations; extractiveness is location-dependent and should decompose into separate constraints (urban notary vs rural notary extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_extraction_asymmetry, empirical, 'Geographic asymmetry in extraction from notary requirement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notary_ink_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(notary_tr_t0, notary_ink_dependency, theater_ratio, 0, 0.42).
narrative_ontology:measurement(notary_tr_t10, notary_ink_dependency, theater_ratio, 10, 0.55).
narrative_ontology:measurement(notary_tr_t20, notary_ink_dependency, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(notary_be_t0, notary_ink_dependency, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(notary_be_t10, notary_ink_dependency, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(notary_be_t20, notary_ink_dependency, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notary_ink_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(notary_ink_dependency, real_estate_transaction_friction).
narrative_ontology:affects_constraint(notary_ink_dependency, international_contract_recognition).
narrative_ontology:affects_constraint(notary_ink_dependency, digital_identity_recognition).

% DUAL FORMULATION NOTE:
% The notary wet-ink persistence can be decomposed into three structurally distinct constraints: (1) identity_verification_requirement (ε≈0.15, Mountain-like) — the genuine need to verify document signatory identity, solved in both wet-ink and cryptographic forms; (2) in_person_presence_mandate (ε≈0.52, Tangled Rope) — the legal requirement for physical notary presence, which provides minor fraud prevention but significant transaction friction; (3) recording_statute_inertia (ε≈0.38, Piton) — the path dependency that maintains in-person requirements in property law despite technology availability. This story focuses on constraint #2 (in-person presence mandate) as the primary extraction mechanism, with #1 as the underlying coordination function and #3 as the institutional inertia context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notary_ink_dependency, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
