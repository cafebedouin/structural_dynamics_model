% ============================================================================
% CONSTRAINT STORY: notary_ink_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The notary/wet-ink requirement for high-value legal documents represents
 *   a persistent institutional constraint on digital transaction
 *   infrastructure. Originating in 17th-century notarial practice for
 *   international commerce (where physical seals and ink provided
 *   tamper-evidence in pre-cryptography contexts), the requirement has
 *   survived the emergence of cryptographic non-repudiation, digital identity
 *   systems, and blockchain-based title registries. The constraint exhibits a
 *   dual structure: it functions genuinely as a coordination mechanism
 *   (notaries provide identity verification and create a clear liability
 *   assignment), but it also functions extractively (the notary industry
 *   captures economic rent by maintaining demand for their services; remote
 *   parties bear compliance friction; platform operators must maintain legacy
 *   infrastructure). The constraint's theater_ratio (0.68) reflects that much
 *   of the notarial ritual is performative — the official seal, journal
 *   entry, and formal declaration ceremony have minimal epistemic function
 *   but persist as tradition. The theater has increased over the interval as
 *   digital alternatives have become technically available, making the
 *   wet-ink requirement less functionally justified and more clearly
 *   theatrical. This is a diagnostic case of how institutional inertia
 *   ('we've always required wet ink') can maintain a constraint even when the
 *   technical and economic arguments for its sunset are overwhelming.
 *
 * KEY AGENTS:
 *   - Remote Transaction Party: Primary victim (powerless/trapped) — bears full compliance cost of geographic/scheduling friction; no legitimate exit option
 *   - Notary Industry: Primary beneficiary (institutional/arbitrage) — captures stable economic rent from wet-ink requirement; could transition to digital services but benefits from regulatory protection
 *   - Transaction Platform Operators: Secondary victim (moderate/constrained) — experience friction from wet-ink requirement but also benefit from notary coordination ritual that provides liability clarity
 *   - Digital Identity / Blockchain Coalition: Organized agents (organized/constrained) — building alternative infrastructure with sunset logic; see the constraint as temporary friction point
 *   - State Notary Licensing Regime: Institutional actor (institutional/arbitrage) — maintains regulatory requirement primarily through inertia; regime sees its own function as degraded but lacks political will to update
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing regulatory convenience as legal necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notary_ink_dependency, 0.38).
domain_priors:suppression_score(notary_ink_dependency, 0.62).
domain_priors:theater_ratio(notary_ink_dependency, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notary_ink_dependency, extractiveness, 0.38).
narrative_ontology:constraint_metric(notary_ink_dependency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(notary_ink_dependency, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notary_ink_dependency, tangled_rope).
narrative_ontology:human_readable(notary_ink_dependency, "The Notary/Wet-Ink Persistence").
narrative_ontology:topic_domain(notary_ink_dependency, "legal/institutional").

domain_priors:requires_active_enforcement(notary_ink_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notary_ink_dependency, notary_industry).
narrative_ontology:constraint_beneficiary(notary_ink_dependency, incumbent_legal_service_providers).
narrative_ontology:constraint_victim(notary_ink_dependency, transaction_parties_remote).
narrative_ontology:constraint_victim(notary_ink_dependency, epistemic_verification_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REMOTE TRANSACTION PARTY (SNARE) — Cannot execute high-value legal documents (power of attorney, property transfer, loan origination) without in-person presence before a notary. Geographic and scheduling constraints create total extraction: this agent bears full compliance cost with no exit option. The in-person requirement persists even when digital identity verification and cryptographic signatures are technically superior.
constraint_indexing:constraint_classification(notary_ink_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRANSACTION PLATFORM OPERATOR (TANGLED ROPE) — Digital platforms (online mortgage brokers, real estate marketplaces, title companies) want to reduce in-person friction to increase transaction volume. The notary requirement constrains them (reduces conversion, increases operational cost, creates geographic friction). But the platforms also benefit from the notary constraint as a coordination mechanism — it provides a socially accepted verification ritual that assigns liability clearly. The constraint is both extractive (slows adoption) and coordinative (establishes transaction legitimacy). Suppression is high (firms cannot easily work around notary requirements), but not total (some transaction types allow digital signatures under UETA/ESIGN).
constraint_indexing:constraint_classification(notary_ink_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NOTARY INDUSTRY (ROPE) — Directly benefits from the wet-ink persistence requirement. Notaries have stable demand, set fees, and capture a portion of every high-value transaction. The constraint operates as a coordination mechanism for this agent — it provides a legitimate, state-regulated verification ritual that assigns clear liability and preserves the notary's economic role. Exit options are strong (notaries could pivot to other verification services), but the current constraint creates a protected rent stream with minimal competition from digital alternatives.
constraint_indexing:constraint_classification(notary_ink_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL IDENTITY / BLOCKCHAIN COALITION (SCAFFOLD) — Organized technical actors (NIST digital identity standards, blockchain notarization systems, distributed ledger title registries) see the wet-ink requirement as a temporary coordination failure with a clear sunset. Decentralized identity credentials, cryptographic signature verification, and immutable transaction records can replace the notary ritual entirely. The coalition is building alternative infrastructure that reduces theater (no performative presence ritual required) and provides superior tamper-evidence. Sunset: estimated 15-20 years as digital identity standards mature and regulatory acceptance spreads.
constraint_indexing:constraint_classification(notary_ink_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE NOTARY LICENSING REGIME (PITON) — The institutional requirement for notary presence persists primarily through regulatory inertia. States maintain notary licensing requirements and wet-ink signature mandates not because they provide superior verification (they don't — cryptographic signatures are more tamper-evident), but because the regulatory infrastructure is entrenched. The theater ratio is high: the notarial ritual (official seal, journal entry, formal declaration) is largely performative. The actual epistemic work (identity verification) could be done more reliably through digital means, but the ritual persists because state legislatures have not updated the legal framework, and the notary lobby opposes digital alternatives. The regime sees its own function as degraded but maintains it through institutional momentum.
constraint_indexing:constraint_classification(notary_ink_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, this perspective risks naturalizing the wet-ink requirement as inherent to legal authenticity. The framing: 'Physical signatures and notarial seals are intrinsically more legally binding than digital signatures.' However, this is a false summit. The structural data reveals contingency, not natural law: cryptographic signatures are technically superior to wet ink in every measurable way (tamper-evidence, non-repudiation, auditability, speed). The belief that wet ink is inherently more 'authentic' is a legal convention, not a physical law. The engine will flag this perspective as naturalization of a contingent institutional preference.
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

test(piton_threshold) :-
    domain_priors:theater_ratio(notary_ink_dependency, TR),
    TR >= 0.70.

:- end_tests(notary_ink_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from remote parties (who must travel or engage expensive digital notary services) and from platform operators (who must maintain wet-ink infrastructure). But the extraction is not severe as measured by absolute economic cost — most high-value transactions proceed despite the friction, and digital notarization workarounds exist in many jurisdictions under UETA/ESIGN. The measured value reflects that the notary requirement increases transaction cost but does not prevent most transactions from completing. Suppression (0.62): High. While digital alternatives exist, regulatory barriers prevent their full adoption: state laws explicitly require wet-ink signatures for property transfers, loan documents, powers of attorney, and other high-value instruments. Parties cannot easily escape the requirement through choice or arbitrage — the requirement is jurisdiction-enforced. Theater ratio (0.68): High and increasing. The notarial ritual (seal, oath, journal entry) serves minimal verification function beyond what digital identity verification could provide. The theater has increased over the interval as the gap between what cryptography can do and what the ritual requires has widened. The ritual persists not because it works better, but because legal convention and regulatory inertia maintain it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows classic perspectival collapse. The remote party sees a pure snare — total extraction with no exit. The notary industry sees coordination and rent protection (rope). The platform operator experiences both (tangled rope). The digital coalition sees a temporary problem being solved (scaffold) with clear sunset dynamics. The state regime sees its own ritual as degraded and performative (piton) but maintains it through institutional momentum. The analytical observer risks naturalizing the wet-ink requirement as inherent to legal authenticity (false mountain). The core gap: whether the constraint is a natural feature of contract law (mountain hypothesis) or a contingent institutional choice (snare/scaffold/piton hypothesis). The structural data strongly supports contingency — cryptographic signatures are technically superior, economically efficient, and legally equivalent under UETA, yet the constraint persists. This is not natural law; it is regulatory inertia. The gap narrows as digital alternatives proliferate and regulatory capture becomes more visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. The notary industry (institutional/arbitrage) has low d — they benefit from the constraint and can exit to other services if desired, but choose to maintain wet-ink focus. Remote transaction parties (powerless/trapped) have high d — they bear full compliance cost with no exit option. Platform operators (moderate/constrained) have medium d — they experience the constraint as extractive (forced to maintain wet-ink infrastructure) but also benefit from the notary coordination function (provides liability assignment). The digital coalition (organized/constrained) has medium-high d — they see the constraint as a barrier but have some exit capacity through federation and parallel infrastructure building. The state licensing regime (institutional/arbitrage) appears as beneficiary on the surface but acts as victim once regulatory capture is factored in — the regime's own function is degraded by maintaining a requirement it recognizes as obsolete. This suggests a directionality override: the state regime's d should be higher (0.35-0.45) than pure institutional/arbitrage derivation would suggest, capturing that it is partly captured by notary lobbying and partly victimized by the regulatory anachronism it maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy arises from the tension between the notary constraint's genuine coordination function (assigning clear liability through identity verification) and its extractive effects (capturing notary industry rent, imposing friction on remote parties, forcing platform operators to maintain legacy infrastructure). The tangled rope classification resolves this by acknowledging both: the constraint is genuinely coordinative (notaries do verify identity and create transaction clarity) AND genuinely extractive (it maintains notary economic rent and imposes friction that has no epistemic justification). The theater_ratio (0.68) anchors the mandatrophy: the more the notarial ritual is performative (theater rising), the more the rope classification is a false positive and the tangled rope / snare perspective becomes correct. The sunset logic (scaffold perspective) provides structural resolution: as digital identity and blockchain infrastructure mature, the coordination function can be satisfied without the wet-ink requirement, the theater drops, and the constraint shifts from tangled rope to pure snare (during regulatory lag, agents still trapped in wet-ink requirement despite superior alternatives) and eventually to historical piton (wet-ink requirements persist in some jurisdictions through pure inertia even after digital alternatives are universal).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_authenticity_definition,
    'Does legal authenticity require physical presence and wet-ink signatures, or is it a property of cryptographic non-repudiation that can be equally satisfied by digital means?',
    'Comparative analysis of court cases: how frequently do disputes about document authenticity turn on the presence of wet ink vs. on the verifiability of the signer''s identity and intent? Cross-jurisdictional review of UETA/ESIGN implementation and litigation outcomes.',
    'If wet ink is legally privileged by courts due to evidentiary tradition: notary requirement may be mountain-like (legal precedent force). If digital signatures are treated equivalently when properly executed: requirement is purely regulatory contingency (snare/tangled rope). This determines whether the constraint can sunset or is reinforced by judicial habit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_authenticity_definition, empirical, 'Whether legal authenticity is intrinsically tied to wet ink or can be satisfied by cryptographic non-repudiation').

omega_variable(
    identity_verification_equivalence,
    'Are government-issued digital identity credentials (e.g., NIST Level 3 digital ID) as reliable as in-person notary identification verification for preventing fraud and impostor signatures?',
    'Comparative fraud rates: notarized documents with impostor signatures discovered post-execution vs. digitally signed documents with failed digital ID verification. Analysis of identity theft and signature fraud in jurisdictions with UETA-compliant digital notarization.',
    'If digital identity is equally or more reliable: the suppression value drops significantly (agents can exit via digital notarization). Constraint shifts from Snare to Tangled Rope or Rope. If in-person verification catches more fraud: suppression remains high and the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_verification_equivalence, empirical, 'Whether digital identity verification provides equivalent or superior fraud prevention vs. in-person notary ID checks').

omega_variable(
    regulatory_capture_strength,
    'How much of the wet-ink requirement''s persistence is due to notary industry lobbying vs. genuine epistemic preference for physical verification?',
    'Legislative history analysis: tracking state notary law revisions, lobby spending records, and legislative testimony. Comparison of states that have adopted UETA/E-SIGN broadly vs. states with restricted digital notarization. Voter preference surveys for digital vs. wet-ink alternatives in states where choice is available.',
    'If regulatory capture is primary driver: the piton perspective dominates and the constraint has no mountain-like force. The sunset is accelerating and market-driven (digital adoption will outpace regulation). If epistemic preference is primary: the mountain perspective has some legitimate basis and the constraint may be more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_strength, empirical, 'The relative strength of notary industry lobbying vs. genuine epistemic preference in maintaining wet-ink requirements').

omega_variable(
    transaction_friction_cost,
    'What is the aggregate economic cost of the wet-ink requirement? What volume of transactions is abandoned or delayed due to notarization friction?',
    'Economic analysis of transaction abandonment rates before/after jurisdictions implement digital notarization. Cross-border transaction data: do parties route transactions through digital-friendly jurisdictions? Lending market analysis: how much mortgage origination friction is attributable to wet-ink requirements?',
    'If costs are high (>1% of transaction value): the extractiveness value (0.38) is conservative and should be raised. The constraint becomes more clearly exploitative. If costs are low (<0.1%): the snare classification may be overstated and the rope/tangled rope classifications dominate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transaction_friction_cost, empirical, 'The aggregate economic cost of wet-ink persistence and transaction abandonment due to notarization friction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notary_ink_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(notary_tr_t0, notary_ink_dependency, theater_ratio, 0, 0.55).
narrative_ontology:measurement(notary_tr_t5, notary_ink_dependency, theater_ratio, 5, 0.62).
narrative_ontology:measurement(notary_tr_t10, notary_ink_dependency, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(notary_be_t0, notary_ink_dependency, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(notary_be_t5, notary_ink_dependency, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(notary_be_t10, notary_ink_dependency, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notary_ink_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(notary_ink_dependency, remote_closing_friction).
narrative_ontology:affects_constraint(notary_ink_dependency, digital_identity_standardization).
narrative_ontology:affects_constraint(notary_ink_dependency, title_registry_blockchain_adoption).

% DUAL FORMULATION NOTE:
% The notary requirement has two structurally distinct aspects: (1) identity verification for preventing fraud/impersonation, and (2) regulatory ritual for assigning liability. Constraint story notary_ink_dependency focuses on the regulatory persistence of wet-ink requirement despite digital identity alternatives. Upstream dependency: digital_identity_standardization (if NIST Level 3 digital identity achieves 99.9% fraud prevention, notary requirement loses epistemic justification). Downstream dependents: remote_closing_friction and title_registry_blockchain_adoption (both require notary requirement to sunset before they can fully deploy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notary_ink_dependency, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
