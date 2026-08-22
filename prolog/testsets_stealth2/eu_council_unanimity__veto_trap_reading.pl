% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity Rule — Veto-Trap Reading (Minoritarian Extraction via Credible Blocking Threats)
 *   domain: political/institutional/international-relations
 *
 * SUMMARY:
 *   Under the veto-trap reading, the Council's unanimity requirement in
 *   designated domains (taxation, foreign policy, treaty change, enlargement,
 *   emergency financial instruments) operates as a structural vulnerability:
 *   any single member state can credibly threaten to block collective action,
 *   and the majority coalition's rational response is to purchase the
 *   blocker's consent with opt-outs, budget corrections, side payments,
 *   sequencing concessions, or dilution of the common position. The
 *   historical record this reading rests on includes the 1984 British budget
 *   correction won under blocking pressure, successive Danish and Irish
 *   opt-out packages, the December 2011 British veto that forced the fiscal
 *   compact onto an intergovernmental track, the multi-year paralysis of
 *   rule-of-law responses and Ukraine-aid decisions under single-state holds,
 *   bilateral accession gatekeeping (Cyprus over Turkey, Bulgaria over North
 *   Macedonia), and the dilution of the 2024 migration pact's solidarity
 *   mechanics. Each resolved episode sets a precedent that raises the
 *   expected payoff of the next block, which is why the reading treats the
 *   arrangement as accumulating rather than static. KEY AGENTS (by structural
 *   relationship): - veto_wielding_member_states: Primary beneficiary
 *   (institutional/constrained) — converts treaty-guaranteed blocking power
 *   into concessions - coalition_majority_member_states: Primary target
 *   (institutional/constrained) — holds preferences neutralized by
 *   single-state holds - blocking_state_incumbent_leaderships: Secondary
 *   beneficiary (moderate/identity_locked) — converts blocking into domestic
 *   political capital - policy_target_populations: Diffuse target
 *   (powerless/trapped) — bear the cost of blocked and diluted policy -
 *   european_commission: Institutional intermediary bearing agenda-control
 *   losses (institutional/constrained) - council_presidency_rotations: Agenda
 *   administrator running the consensus machinery (institutional/constrained)
 *   - candidate_accession_states: Excluded voice (moderate/trapped) — gated
 *   by holds they cannot answer - integration_treaty_scholars: Analytical
 *   observer (analytical/analytical) FAMILY NOTE: this file instantiates one
 *   reading of the eu_council_unanimity kernel. Sibling files instantiate the
 *   sovereignty-guarantor and diplomatic-capital readings, which assess the
 *   same standing arrangement by their own lights and author materially lower
 *   epsilon in their own stories; per the epsilon-invariance principle those
 *   values belong to those files, and this file's epsilon (0.74) is authored
 *   only from the veto-trap reading's assessment of the arrangement as it
 *   operates.
 *
 * KEY AGENTS:
 *   - veto_wielding_member_states: primary beneficiary (institutional/constrained) — holds the credible blocking threat and collects the concessions
 *   - coalition_majority_member_states: primary target (institutional/constrained) — same nominal power level as the blocker, differentiated by role and exit, pays in dilution, delay, and side payments
 *   - blocking_state_incumbent_leaderships: secondary beneficiary (moderate/identity_locked) — national governments whose governing identity is fused to the confrontational stance
 *   - policy_target_populations: diffuse target (powerless/trapped) — aid recipients, asylum seekers, and populations under rights erosion who absorb blocked-policy costs
 *   - european_commission: institutional intermediary (institutional/constrained) — loses agenda-setting control in unanimity domains, gains brokerage indispensability
 *   - council_presidency_rotations: agenda administrator (institutional/constrained) — chairs, brokers, and stages the consensus process on six-month horizons
 *   - candidate_accession_states: excluded voice (moderate/trapped) — accession aspirants whose entry any single member can halt
 *   - integration_treaty_scholars: analytical observer (analytical/analytical) — maps the arrangement's evolution without holding a material position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.74).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.66).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity Rule — Veto-Trap Reading (Minoritarian Extraction via Credible Blocking Threats)").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "political/institutional/international-relations").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'db2b0030-1eff-4d23-ba67-5490aa7fa6e1').
narrative_ontology:cs_kernel_codification('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', formalized).
narrative_ontology:cs_authority_grounding('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', lineage).
narrative_ontology:cs_interpretation_layer_present('db2b0030-1eff-4d23-ba67-5490aa7fa6e1').
narrative_ontology:cs_reading_relation('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', foundational, credible_blocking_transfers_value_to_blocker).
narrative_ontology:cs_axiom_status(credible_blocking_transfers_value_to_blocker, holdable).
narrative_ontology:cs_axiom_grounding('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', credible_blocking_transfers_value_to_blocker, empirically_contingent).
narrative_ontology:cs_axiom('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', secondary, formal_consensus_masks_coerced_concession).
narrative_ontology:cs_axiom_status(formal_consensus_masks_coerced_concession, holdable).
narrative_ontology:cs_axiom_grounding('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', formal_consensus_masks_coerced_concession, empirically_contingent).
narrative_ontology:cs_reference_frame('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', luxembourg_compromise_consent_settlement).
narrative_ontology:cs_drift_state('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', contemporary_holdup_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('db2b0030-1eff-4d23-ba67-5490aa7fa6e1', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, veto_wielding_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_state_incumbent_leaderships).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, policy_target_populations).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, european_commission).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, european_commission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Member governments holding treaty-guaranteed blocking power in designated Council domains. They convert the threat to withhold consent into opt-outs, budget corrections, policy dilution, and sequencing wins, and each success makes the next attempt cheaper to mount. Leaving the union altogether is available in principle but ruinously costly and unnecessary while the blocking position pays; remaining inside is the profit-maximizing posture.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, veto_wielding_member_states, beneficiary,
    institutional, biographical, constrained, continental).

% The twenty-plus governments whose preferred common policies are neutralized by a single state's refusal of consent. Their options are buying the blocker out, diluting the policy until the blocker consents, delaying indefinitely, or pursuing costly workarounds such as enhanced cooperation or intergovernmental agreements outside the treaties. They cannot outvote in the covered domains, and exiting the union would forfeit far more than the individual blocked policy is worth.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority_member_states, payer,
    institutional, generational, constrained, continental).

% National governing leaderships whose political identity is built on confronting the union's institutions. Every concession won validates the strategy before their domestic electorate, and every retreat would read as capitulation to their base. Abandoning the confrontational stance is politically lethal at home even when the substantive demands have already been met, so the stance persists past its own victories.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_state_incumbent_leaderships, beneficiary,
    moderate, immediate, identity_locked, national).

% People whom blocked or diluted common policies would have served: recipients of delayed aid, asylum seekers governed by weakened solidarity mechanics, and citizens living under rights erosion that collective responses failed to address. They chose none of the holds that delay their relief, and they cannot exit the circumstances that make the policy urgent.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, policy_target_populations, payer,
    powerless, immediate, trapped, continental).

% The union's executive, which proposes and manages common policy but cannot set the final terms in unanimity domains, where its agenda-setting authority is effectively suspended by any single government's hold. It loses agenda-control rents yet becomes more indispensable as the broker shuttling between the blocker and the majority, so its net position is a loss of power compensated partly by centrality.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, european_commission, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, european_commission, beneficiary).

% Successive rotating presidencies that chair meetings, run the pre-negotiation machinery, broker packages, and stage the public face of consensus. They administer the process without owning its rule, absorb the blame for failures, and operate on six-month horizons that discourage any challenge to the structural arrangement they serve.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, council_presidency_rotations, agenda_setter,
    institutional, biographical, constrained, continental).

% Accession aspirants whose entry into the union can be halted by any existing member pursuing a bilateral dispute. They would argue against unilateral gatekeeping of a shared enlargement process, but they sit outside the Council chamber where the decision is made, and their alternative — walking away from accession — forfeits the goal they have organized their reforms around for years.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, candidate_accession_states, excluded,
    moderate, generational, trapped, regional).

% Scholars of integration and treaty law who trace the arrangement's evolution from its post-crisis settlement through its current operation. They publish analyses of blocking episodes and concession patterns, take no material position in the bargaining, and can see the whole structure that participants each view from one seat.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, integration_treaty_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, veto_wielding_member_states).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces common positions that all member states formally consent to, solving the problem of joint action among sovereign equals who will not accept being bound against their will; the consent requirement keeps all twenty-seven governments committed to implementing whatever is agreed.
% TRANSFER_FUNCTION: Moves policy value from the majority coalition's preferred outcome to the blocking state's demanded position — through opt-outs, budget corrections, side payments, sequencing concessions, and dilution of common positions — and moves discretionary authority from supranational institutions back to national capitals.
% ABSENT_VOICES: Candidate countries awaiting accession would object to unilateral gatekeeping of enlargement but are outside the chamber; populations targeted by blocked policies have no seat; future majorities that will want qualified-majority efficiency are represented only by today's advocates. Inside the room, dissent is further muted by consensus norms that treat open opposition as breach of etiquette.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement vanished overnight, covered domains would shift to qualified-majority voting, blocking states would lose their leverage stream, the accumulated opt-out and rebate architecture would lose its protecting shadow and come under majority attack, and integration would accelerate in domains currently frozen — the entire bargaining equilibrium among the twenty-seven would reorganize around coalitional vote-counting.
% FOUNDING_PROBLEM: After the 1965 Empty Chair Crisis, the member states needed a settlement guaranteeing that no sovereign state would ever be bound by collective action against its explicit consent — the Luxembourg Compromise embedded that guarantee, and subsequent treaty drafts preserved unanimity in domains touching core national interests.
% FOUNDING_PROBLEM_CORROBORATION: The historical record of the Empty Chair Crisis and the Luxembourg Compromise, and the treaty scholarship built on it, corroborates the founding problem from outside today's beneficiary set — the guarantee was designed as protection, and contemporaneous accounts attest that purpose. However, no seat outside the current benefiting structure attests that the founding problem remains live in its original form: liveness is asserted by member governments from within the arrangement they benefit from, while the documented concession stream supports the converted-function reading. That split attestation is itself the signal behind the contested status.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.74 at interval end) because the transfer channel is systematic, not incidental: opt-outs, budget corrections, and dilution events recur across decades and domains, and each resolved episode prices future blocks. Suppression (0.66) reflects the majority coalition's option structure rather than physical coercion: the majority cannot outvote in covered domains, and its alternatives (enhanced cooperation, intergovernmental tracks outside the treaties, abandonment) are legal but costly and reputationally fraught, which suppresses first-preference pursuit without eliminating it. Theater ratio (0.35) captures the growing share of summit activity that stages unity over pre-purchased outcomes — communiques presenting coerced results as collective will, Article 7 hearings that function as ritual rather than remedy — while the underlying brokering function remains real. Accessibility collapse (0.52): alternatives are known and usable (enhanced cooperation has been activated several times) but carry two-speed-Europe stigma and legal fragility, so they collapse only partially once the arrangement's dynamics are understood. Resistance (0.60) is sustained and institutionalized: repeated passerelle-clause initiatives, the 2020 rule-of-law conditionality mechanism, litigation over budget conditionality, and open political campaigns for qualified-majority extension in new domains. The three measurement series share one six-point grid (T0 approx 1994 post-Maastricht through T30 approx 2024); the trajectories are monotonic ratchets, not cycles — the dynamic is precedent accumulation, so no cyclical-pattern machinery applies. The suppression_requirement series is authored deliberately: the story traces enforcement-intensification, as holding the arrangement together has required ever-heavier procedural machinery (sherpa layers, Coreper pre-cooking, presidency brokering intensity) and defensive counter-tools against rising resistance; a flat series would misrepresent that. Claim and metrics are independent authored facts: the claim of tangled_rope reflects the judgment that genuine coordination output (jointly consented positions that all 27 implement) and asymmetric transfer coexist in one structure; the metrics describe the arrangement's actual operation without being tuned to any predicted engine verdict.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the coalition-majority seat the arrangement presents as hostage-taking: a minority position overriding a majority preference through a threat the majority cannot call. From the blocking-state seat the same structure presents as legitimate insurance and leverage — the guaranteed right not to be bound against consent, exercised for national advantage. The incumbent-leadership seat adds identity fusion: the stance is constitutive of the government's political brand, so the concession stream reads as vindication rather than cost imposed on others. The excluded candidate-country seat experiences the arrangement as a wall operated by others. The engine computes this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: veto_wielding_member_states and blocking_state_incumbent_leaderships sit near the full-beneficiary end (d near 0), so effective extraction inverts into subsidy for them — the arrangement pays them. Victim declarations place coalition_majority_member_states, policy_target_populations, and european_commission near the full-target end (d near 1), amplified by constrained or trapped exits: the majority cannot outvote or cheaply leave, target populations cannot exit the situations the blocked policies address. Two nuances deserve note. First, the same-level structure: the majority and the blocker hold the identical power atom (institutional) — the differentiation that produces the extraction is the veto itself plus asymmetric exit value, not a power gradient, which is why no directionality override is needed or appropriate here. Second, the Commission is declared a victim on its net position (agenda-control losses exceed brokerage gains), so its derived d sits high; the brokerage offset is recorded qualitatively here rather than as an override, since the override surface keys on power atoms and would misfire across the other institutional seats. Incumbent identity lock stabilizes the beneficiary position rather than amplifying extraction — lock deepens chi only for targets, and here it entrenches the seat collecting the gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assuring sovereign states emerging from the Empty Chair Crisis that collective membership would never bind them against their will — is genuinely contested in liveness: states still invoke it, but the arrangement's operative center of gravity has migrated from consent protection to concession collection. The tangled_rope claim is what prevents both mislabels. Reading the arrangement as pure extraction (snare) would erase the real coordination output: jointly consented positions that all member states implement, and the continued presence of all 27 at the table, which no majority-voting counterfactual reproduces for free. Reading it as pure coordination (rope) would erase the systematic transfer channel and its precedent ratchet. The R5 interview sharpens this: founding_problem_status is contested and disappearance_verdict is world_rearranges, so the mismatch consumer does not fire a dead-mandate zombie flag automatically — but the contested status routes investigation to the precedent-ratchet omega, which is where a dead-or-live determination will actually be made. If qualified-majority extension succeeds in enough domains, the mandate question resolves as dead-but-retained in the residue domains; if the ratchet continues, the arrangement resolves as live-but-converted, with the original warrant serving as cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the standing unanimity arrangement accurately captured by this veto-trap reading (a structural vulnerability that enables minoritarian extraction), or by the sibling readings of the eu_council_unanimity kernel (sovereignty guarantee, diplomatic capital)?',
    'Comparative structural audit across unanimity-domain decisions: code each concluded act for the presence of blocking episodes, purchased opt-outs, side payments, and policy dilution versus cases where no state''s position moved under threat. Whichever reading explains the outcome distribution without residual anomalies is the structurally accurate instantiation.',
    'The sibling readings would author substantially lower epsilon over the same referent (consent-protection and legitimacy-building framings see coordination cost, not transfer) and classify toward rope; this file''s high epsilon and tangled_rope claim stand only if blocking-driven transfer dominates the outcome distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame routing of the intra-kernel reading contest: this constraint is one reading of eu_council_unanimity, and the disagreement is located in the normative status of blocking threats.').

omega_variable(
    concession_transfer_rate,
    'What fraction of formally unanimous Council outcomes in unanimity-designated domains embed material value transferred to a blocking state (opt-outs, budget corrections, dilution, sequencing wins) rather than reflecting convergent member-state preferences?',
    'Coded longitudinal dataset of unanimity-domain legislative and strategic decisions, tracking pre-decision threat episodes, documented opt-out acquisitions, side payments, and dilution events against a baseline of preference convergence.',
    'Transfer rates persistently above roughly 0.4 confirm the high epsilon authored here and support watching for snare drift; rates below roughly 0.2 would indicate this reading overstates the arrangement and the diplomatic-capital sibling better fits the data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concession_transfer_rate, empirical, 'Empirical rate of concession-bearing outcomes inside formally unanimous results.').

omega_variable(
    precedent_ratchet_trajectory,
    'Does each successful blocking episode raise the expected payoff of future blocking (precedent accumulation), producing a ratchet in which the arrangement migrates from hybrid coordination-plus-transfer toward pure extraction?',
    'Extend the authored measurement grid forward: continued rise of base_extractiveness past roughly 0.8 accompanied by theater_ratio above 0.5 confirms the ratchet; plateau or reversal following qualified-majority extensions refutes it.',
    'Confirmation predicts a tangled_rope-to-snare transition for this seat structure; refutation stabilizes the tangled_rope classification and shifts attention to the durability omega below.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_ratchet_trajectory, empirical, 'Whether extraction accumulates monotonically through precedent or self-limits.').

omega_variable(
    incumbent_identity_lock_durability,
    'Is the blocking-and-concession mechanism dependent on identity-locked incumbent leaderships in high-blocking states, or does it survive leadership turnover as a structural state-level incentive available to any government?',
    'Natural experiment via electoral turnover in states with established blocking reputations: if successor governments sustain the veto strategy, the mechanism is structural; if they abandon it, the mechanism was fused to particular governing identities.',
    'Structural persistence supports long-horizon snare drift regardless of personnel; identity-dependence implies self-limiting cycles tied to domestic political generations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incumbent_identity_lock_durability, empirical, 'Durability of the mechanism across leadership turnover in blocking states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecu_veto_trap_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ecu_veto_trap_tr_t6, eu_council_unanimity__veto_trap_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(ecu_veto_trap_tr_t12, eu_council_unanimity__veto_trap_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(ecu_veto_trap_tr_t18, eu_council_unanimity__veto_trap_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(ecu_veto_trap_tr_t24, eu_council_unanimity__veto_trap_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(ecu_veto_trap_tr_t30, eu_council_unanimity__veto_trap_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(ecu_veto_trap_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ecu_veto_trap_be_t6, eu_council_unanimity__veto_trap_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(ecu_veto_trap_be_t12, eu_council_unanimity__veto_trap_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(ecu_veto_trap_be_t18, eu_council_unanimity__veto_trap_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(ecu_veto_trap_be_t24, eu_council_unanimity__veto_trap_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(ecu_veto_trap_be_t30, eu_council_unanimity__veto_trap_reading, base_extractiveness, 30, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(ecu_veto_trap_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ecu_veto_trap_su_t6, eu_council_unanimity__veto_trap_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(ecu_veto_trap_su_t12, eu_council_unanimity__veto_trap_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(ecu_veto_trap_su_t18, eu_council_unanimity__veto_trap_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(ecu_veto_trap_su_t24, eu_council_unanimity__veto_trap_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(ecu_veto_trap_su_t30, eu_council_unanimity__veto_trap_reading, suppression_requirement, 30, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'EU Council unanimity'. The single label covers structurally distinct claims that must not share one epsilon: this file (veto_trap_reading) assesses the standing arrangement as a hold-up structure and authors high epsilon (0.74); the sovereignty_guarantor_reading file assesses the same arrangement as consent protection and authors low epsilon; the diplomatic_capital_reading file assesses it as legitimacy-building discipline and authors low-to-moderate epsilon. Each file carries its own beneficiaries, victims, metrics, and classification; the family link here records that the readings compete over one kernel and that this reading's documented extraction episodes create downstream legitimacy pressure on the diplomatic-capital sibling. Upstream/downstream: the sovereignty-guarantor settlement (Luxembourg Compromise lineage) is the historical upstream from which this reading's extraction infrastructure evolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
