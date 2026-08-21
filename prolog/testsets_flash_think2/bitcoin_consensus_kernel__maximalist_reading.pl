% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__maximalist_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Bitcoin Maximalist Reading of Monetary Immutability
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint story instantiates the 'maximalist_reading' of the
 *   'bitcoin_consensus_kernel'. It describes the belief system and associated
 *   social/technical enforcement that views Bitcoin's whitepaper as
 *   establishing an immutable monetary policy and core protocol, where any
 *   change is considered a violation of a founding covenant. This reading
 *   prioritizes scarcity and censorship-resistance above all else, often at
 *   the expense of scalability or new features. The constraint is claimed as
 *   a Tangled Rope because it genuinely coordinates a decentralized network
 *   around a shared monetary policy, but does so with significant asymmetric
 *   extraction from those seeking protocol evolution, enforced by a powerful,
 *   identity-locked community.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.85).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.9).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Reading of Monetary Immutability").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '3d51b7fb-76e2-4911-a11e-71ce1f9f470e').
narrative_ontology:cs_kernel_codification('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', fixed_text).
narrative_ontology:cs_authority_grounding('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', lineage).
narrative_ontology:cs_interpretation_layer_present('3d51b7fb-76e2-4911-a11e-71ce1f9f470e').
narrative_ontology:cs_reading_relation('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', bitcoin_consensus_kernel__pragmatic_synthesis, forecloses).
narrative_ontology:cs_axiom('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', foundational, monetary_immutability_is_covenant).
narrative_ontology:cs_axiom_status(monetary_immutability_is_covenant, holdable).
narrative_ontology:cs_axiom_grounding('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', monetary_immutability_is_covenant, deontological).
narrative_ontology:cs_axiom('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', foundational, protocol_change_is_violation).
narrative_ontology:cs_axiom_status(protocol_change_is_violation, holdable).
narrative_ontology:cs_axiom_grounding('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', protocol_change_is_violation, conventional).
narrative_ontology:cs_reference_frame('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', whitepaper_founding_principles).
narrative_ontology:cs_drift_state('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', contemporary_scaling_debates, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3d51b7fb-76e2-4911-a11e-71ce1f9f470e', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, maximalist_community).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_innovators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, new_users_seeking_lower_fees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively defends the immutability of Bitcoin's monetary policy and core protocol rules, viewing any deviation as a violation of the founding covenant. They exert significant social and technical pressure to resist changes, often through social media, forums, and developer communities. Their identity is deeply tied to this immutable vision.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, maximalist_community, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the perceived scarcity and censorship-resistance guaranteed by the immutable monetary policy, which underpins Bitcoin's value proposition. They have significant capital invested and often align with the maximalist community to protect their holdings, though they may not be as ideologically identity-locked.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_bitcoin_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Seek to introduce new features or improve core protocol functionality (e.g., privacy, smart contracts) but face strong resistance and social pressure from the maximalist community, making it difficult to gain consensus for changes that might be perceived as altering the 'founding covenant'.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_innovators, payer,
    moderate, biographical, constrained, global).

% Develop solutions (e.g., Lightning Network, sidechains) to address Bitcoin's transaction throughput limitations. While some solutions are tolerated, any proposal that requires changes to the base layer or is seen as compromising decentralization or security faces intense scrutiny and often rejection by the maximalist community, limiting their innovation space.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_developers, payer,
    moderate, biographical, constrained, global).

% Are attracted to Bitcoin's properties but find high transaction fees and slow confirmation times prohibitive for everyday use, a direct consequence of the base layer's limited scalability and resistance to protocol changes. Their options are to use less secure off-chain solutions, pay high fees, or seek alternative cryptocurrencies.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, new_users_seeking_lower_fees, payer,
    powerless, immediate, constrained, global).

% Believe Bitcoin's whitepaper established a minimum viable consensus mechanism that should evolve to maximize its utility as a global payment system. They are often marginalized or dismissed by the maximalist community, finding their proposals for iterative improvement blocked by the immutability narrative.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, utility_reading_advocates, excluded,
    organized, biographical, constrained, global).

% Propose a layered approach where the base layer's monetary rules remain immutable, but upper layers are free to innovate. While seemingly a compromise, their proposals are often viewed with suspicion by maximalists who fear any innovation could eventually compromise the base layer's integrity, leading to their exclusion from core decision-making.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, pragmatic_synthesis_advocates, excluded,
    organized, biographical, constrained, global).

% Study the sociotechnical dynamics of Bitcoin's governance, including the role of the maximalist ideology in shaping protocol development and adoption. They analyze the economic and social costs and benefits of the immutability constraint without being directly subject to its enforcement.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, maximalist_community).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, censorship-resistant, and scarce digital monetary base by enforcing strict rules on supply and protocol changes, allowing participants to coordinate around a shared, unalterable financial foundation.
% TRANSFER_FUNCTION: Transfers the cost of protocol rigidity and limited scalability from early holders and maximalist ideologues (who benefit from the immutability) to protocol innovators, scalability solution developers, and new users seeking lower fees or more features.
% ABSENT_VOICES: Advocates for more flexible monetary policy, rapid protocol evolution, or alternative scaling approaches (e.g., utility_reading_advocates, pragmatic_synthesis_advocates) are actively marginalized or excluded from the core consensus discussion, their proposals often framed as 'attacks' on Bitcoin's integrity.
% DISAPPEARANCE_RATIONALE: If the maximalist reading of monetary immutability vanished overnight, the core value proposition of Bitcoin would be fundamentally altered. The entire cryptoeconomic ecosystem built on this perceived immutability would collapse or radically reconfigure, leading to massive value destruction, a loss of trust in its scarcity, and a scramble for new foundational assets or governance models.
% FOUNDING_PROBLEM: To create a decentralized, censorship-resistant digital cash system with a fixed supply, independent of central authorities and immune to inflationary pressures or arbitrary changes.
% FOUNDING_PROBLEM_CORROBORATION: The maximalist community and many long-term holders attest to the ongoing need for censorship resistance, fixed supply, and protection against 'soft' inflation or protocol capture. Critics (e.g., utility_reading_advocates, analytical_observers) argue that while the original problem was real, the maximalist interpretation now hinders Bitcoin's evolution to address new challenges like global scalability and usability, suggesting the problem has evolved beyond the maximalist framing.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the rigid adherence to immutability imposes substantial costs on those seeking to innovate or scale the protocol, effectively transferring value to those who benefit from the status quo (early holders, maximalist community). Suppression is also very high (0.90) due to the intense social pressure, technical resistance (e.g., refusal to adopt soft forks), and ideological gatekeeping employed by the maximalist community against perceived 'attacks' on the protocol. Theater ratio is low (0.10) because the defense of immutability is a core, actively maintained function, not a performance. Accessibility collapse is high (0.75) as viable alternatives for core protocol changes within the Bitcoin ecosystem are severely limited. Resistance is high (0.70) from developers and users frustrated by the lack of flexibility. The metrics show a slight increase in extractiveness and suppression over time, reflecting the hardening of this stance as Bitcoin's network effects grow.
 *
 * PERSPECTIVAL GAP:
 *   The maximalist community and early Bitcoin holders perceive this constraint as a necessary 'mountain' or 'rope' that preserves Bitcoin's fundamental value proposition, ensuring its integrity and scarcity. From their seat, the costs are justified as the price of true decentralization and censorship-resistance. In contrast, protocol innovators, scalability solution developers, and new users experience it as a 'snare' or 'tangled_rope' that stifles innovation, limits utility, and imposes high transaction costs, viewing the immutability as an ideological barrier rather than a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The maximalist_community (agenda_setter) and early_bitcoin_holders (beneficiary) are positioned as net beneficiaries (low directionality) as they directly profit from the immutability narrative and the resulting asset appreciation. Protocol_innovators, scalability_solution_developers, and new_users_seeking_lower_fees are clear targets (high directionality) as they bear the costs of limited flexibility and high fees. Utility_reading_advocates and pragmatic_synthesis_advocates are excluded, meaning their attempts to influence the constraint are suppressed, placing them effectively as targets of the constraint's enforcement mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Mountain (natural law) or a simple Rope (pure coordination). While it provides a coordination function (stable monetary base), the significant and actively enforced extraction from those seeking change, coupled with the identity-locked nature of its primary defenders, reveals its hybrid nature. The 'founding covenant' narrative serves to legitimize this extraction by framing any deviation as a betrayal, rather than a negotiable policy choice. The persistence of the constraint is tied to the ongoing benefit to early holders and the ideological commitment of the maximalist community, rather than solely to its original coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_natural_vs_constructed,
    'Is the perceived immutability of Bitcoin''s monetary policy a natural consequence of its decentralized consensus mechanism, or a constructed social norm actively enforced by the maximalist community?',
    'Analysis of other decentralized protocols with different governance models: if similar technical constraints lead to different policy outcomes, it suggests a social construction rather than natural law.',
    'If constructed, the constraint''s ''mountain-like'' persistence is revealed as dependent on active social enforcement, shifting its classification closer to a Snare or Tangled Rope for those targeted by its rigidity. If natural, its extractiveness is an unavoidable cost of the technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_natural_vs_constructed, conceptual, 'Ambiguity between natural technical limit and socially enforced norm.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative protocol developments primarily structural (technical difficulty of changing decentralized code) or internalized (ideological commitment and social pressure within the maximalist community)?',
    'Post-exit suppression trajectory: if developers leave Bitcoin for other chains and find similar technical challenges but less social resistance to change, it suggests the Bitcoin suppression is more internalized/social.',
    'If internalized, the constraint''s effective suppression is higher than a purely technical measure suggests, as the target carries the ideological suppression with them, making exit from the maximalist mindset difficult even if technically possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for protocol changes.').

omega_variable(
    maximalist_dominance_sustainability,
    'How sustainable is the maximalist reading''s dominance in the face of growing demand for scalability and new features, and the emergence of competing protocols?',
    'Longitudinal analysis of developer migration patterns, user adoption rates of alternative chains, and shifts in public discourse over the next 5-10 years.',
    'If maximalist dominance erodes, the constraint''s extractiveness and suppression may decrease as alternative readings gain traction, potentially shifting its classification towards a more balanced Rope or even a Piton if its function atrophies but inertia keeps it in place.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maximalist_dominance_sustainability, empirical, 'Future trajectory of maximalist ideological dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitcoin_maximalist_reading_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bitcoin_maximalist_reading_tr_t6, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(bitcoin_maximalist_reading_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(bitcoin_maximalist_reading_tr_t18, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(bitcoin_maximalist_reading_tr_t24, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(bitcoin_maximalist_reading_tr_t30, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(bitcoin_maximalist_reading_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(bitcoin_maximalist_reading_be_t6, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(bitcoin_maximalist_reading_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(bitcoin_maximalist_reading_be_t18, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(bitcoin_maximalist_reading_be_t24, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(bitcoin_maximalist_reading_be_t30, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bitcoin_maximalist_reading_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(bitcoin_maximalist_reading_su_t6, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(bitcoin_maximalist_reading_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(bitcoin_maximalist_reading_su_t18, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(bitcoin_maximalist_reading_su_t24, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(bitcoin_maximalist_reading_su_t30, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_lightning_network_scalability).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_sidechain_innovation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'bitcoin_consensus_kernel'. It is linked to sibling readings (utility_reading, pragmatic_synthesis) which offer alternative interpretations of the whitepaper's authority and the protocol's evolutionary path. Each reading constitutes a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
