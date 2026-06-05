% ============================================================================
% CONSTRAINT STORY: eu_membership_and_exit__sovereignty_restored_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_membership_and_exit__sovereignty_restored_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_membership_and_exit__sovereignty_restored_reading
 *   human_readable: EU Membership and Exit: Sovereignty Restored Reading
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   The sovereignty-restored reading instantiates one claim about EU
 *   membership and exit: that the 2016 referendum and the European Union
 *   (Withdrawal) Acts demonstrated Parliament's retained capacity to revoke
 *   supranational supremacy entirely. This reading holds that what Parliament
 *   pooled through membership law, Parliament could take back through exit
 *   law — and exit proved the doctrine. The constraint arises because the
 *   reading rests on an ambiguous claim: the irreversibility thesis (that EU
 *   membership was constitutional bedrock) has been falsified as a matter of
 *   political fact. But falsifying the irreversibility *claim* does not
 *   settle the conceptual question of whether, during membership,
 *   Parliament's sovereignty was actually hollowed despite nominally
 *   retaining revocation capacity. The reading asserts revocability; exit
 *   proves it. But 'proving revocability' is a different claim from 'proving
 *   sovereignty was never lost.' The kernel contest involves three structural
 *   positions on this question: (1) sovereignty-lost reading — subordination
 *   was real; exit merely restores it; (2) sovereignty-pooled reading —
 *   subordination was illusory because pooling was always Parliament's
 *   continuing choice; (3) sovereignty-restored reading — exit proves pooling
 *   was revocable, therefore the doctrine of pooled choice (not pooled loss)
 *   is vindicated. The constraint exhibits Tangled Rope structure because the
 *   reading combines a genuine coordination benefit (supranational rules
 *   reduced transaction costs during membership, benefiting participants)
 *   with asymmetric extraction (exit costs fall on reliance-interest holders
 *   while sovereignty gains accrue to take-back-control coalition).
 *   Extractiveness rises over the interval as the transition from membership
 *   to exit proceeds: stable membership (t0) involves lower structural
 *   extraction; referendum period (t3) mobilizes extraction narratives;
 *   withdrawal acts (t6) implement extraction costs. The theater ratio
 *   reflects that parliamentary sovereignty restoration claims involve
 *   significant performative content — the reading must assert that
 *   Parliament's sovereignty was never truly lost, yet defend against the
 *   counterclaim that subordination was real, all while managing the
 *   distributed institutional cost of demonstrating revocability.
 *
 * KEY AGENTS:
 *   - Take-Back-Control Coalition (Westminster Parliament, Leave campaign): Primary beneficiary (institutional/arbitrage) — regains legislative supremacy, captures narrative of democratic restoration, maintains arbitrage option to reintegrate at lower political cost than trapped cohorts face
 *   - Reliance-Interest Holders (citizens with cross-border life projects, EU-dependent firms, workers exercising freedom of movement): Primary victims (powerless to moderate/trapped to constrained) — bear full cost of exit disruption without capacity to reverse it; bear suppression of alternative coordination pathways
 *   - Integrationist Settlement (EU legal order, supranational regulatory frameworks, devolved governments dependent on EU funding): Secondary victim (institutional/constrained) — loses coordination function and must absorb institutional redesign; constrained by lack of unilateral exit capacity
 *   - Supranational Institutions (EU courts, Commission, Parliament): Institutional actor (institutional/arbitrage) — see doctrine refuted but can shift to looser confederal frames; maintain performative supremacy narratives while acknowledging revocability
 *   - Trade and Regulatory Coalition (states and firms organizing post-exit alignment): Organized agent (organized/constrained) — see constraint as temporary; building alternative coordination via bilateral arrangements and regulatory equivalence; sunset pathway visible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_membership_and_exit__sovereignty_restored_reading, 0.38).
domain_priors:suppression_score(eu_membership_and_exit__sovereignty_restored_reading, 0.48).
domain_priors:theater_ratio(eu_membership_and_exit__sovereignty_restored_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_membership_and_exit__sovereignty_restored_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_membership_and_exit__sovereignty_restored_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(eu_membership_and_exit__sovereignty_restored_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_membership_and_exit__sovereignty_restored_reading, tangled_rope).
narrative_ontology:human_readable(eu_membership_and_exit__sovereignty_restored_reading, "EU Membership and Exit: Sovereignty Restored Reading").
narrative_ontology:topic_domain(eu_membership_and_exit__sovereignty_restored_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(eu_membership_and_exit__sovereignty_restored_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_membership_and_exit__sovereignty_restored_reading, '88a2e42b-a51d-4e38-af0e-b3db43e796c7').
narrative_ontology:cs_kernel_codification('88a2e42b-a51d-4e38-af0e-b3db43e796c7', formalized).
narrative_ontology:cs_authority_grounding('88a2e42b-a51d-4e38-af0e-b3db43e796c7', lineage).
narrative_ontology:cs_interpretation_layer_present('88a2e42b-a51d-4e38-af0e-b3db43e796c7').
narrative_ontology:cs_reading_relation('88a2e42b-a51d-4e38-af0e-b3db43e796c7', eu_membership_and_exit__sovereignty_lost_reading, influences).
narrative_ontology:cs_reading_relation('88a2e42b-a51d-4e38-af0e-b3db43e796c7', eu_membership_and_exit__sovereignty_pooled_reading, influences).
narrative_ontology:cs_axiom('88a2e42b-a51d-4e38-af0e-b3db43e796c7', foundational, parliamentary_supremacy_formally_retained).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_formally_retained, holdable).
narrative_ontology:cs_axiom_grounding('88a2e42b-a51d-4e38-af0e-b3db43e796c7', parliamentary_supremacy_formally_retained, deontological).
narrative_ontology:cs_axiom('88a2e42b-a51d-4e38-af0e-b3db43e796c7', foundational, revocability_empirically_vindicated).
narrative_ontology:cs_axiom_status(revocability_empirically_vindicated, holdable).
narrative_ontology:cs_axiom_grounding('88a2e42b-a51d-4e38-af0e-b3db43e796c7', revocability_empirically_vindicated, empirically_contingent).
narrative_ontology:cs_reference_frame('88a2e42b-a51d-4e38-af0e-b3db43e796c7', parliamentary_constitutional_sovereignty).
narrative_ontology:cs_drift_state('88a2e42b-a51d-4e38-af0e-b3db43e796c7', post_exit_implementation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('88a2e42b-a51d-4e38-af0e-b3db43e796c7', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(eu_membership_and_exit__sovereignty_restored_reading, eu_membership_and_exit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_membership_and_exit__sovereignty_restored_reading, take_back_control_coalition).
narrative_ontology:constraint_beneficiary(eu_membership_and_exit__sovereignty_restored_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_victim(eu_membership_and_exit__sovereignty_restored_reading, integrationist_settlement).
narrative_ontology:constraint_victim(eu_membership_and_exit__sovereignty_restored_reading, reliance_interests_in_supranational_order).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIANCE INTERESTS (SNARE) — Citizens, firms, and workers whose life projects, business operations, and legal status depend on supranational frameworks (freedom of movement, consumer protections, labor standards, environmental rules). Exit extraction is maximal: the cost falls entirely on those trapped by reliance on the pooled order. No meaningful exit option from the disruption; full bearing of transition costs. The extraction is suppressed by exit-vote framing as democratic choice, masking the asymmetry of who pays.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_restored_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INTEGRATIONIST SETTLEMENT (TANGLED ROPE) — The institutional machinery built on supranational supremacy (devolved governments relying on EU funding, regulatory frameworks predicated on harmonization, judicial review conditioned on EU law interpretation) faces extraction through exit. The settlement functioned as coordination: shared rules reduced transaction costs and enabled cross-border activity. But exit demonstration proves the coordination was conditional — the 'irreversible' integration claim was always revocable. The victim set experiences both loss of coordination function AND extraction as the framework is dismantled. Constrained: cannot prevent exit but must absorb adjustment costs.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_restored_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TAKE-BACK-CONTROL COALITION (ROPE) — Parliament, as constitutional sovereign, experiences the constraint as pure coordination restoration. Supremacy of EU law was always Parliament's continuing choice (the pooled reading); exit proves the coalition was right that revocability was real. The extraction runs in the opposite direction from this perspective: Westminster gains sovereignty restoration, regains legislative supremacy, and captures the narrative of democratic agency. Arbitrage: can choose reintegration at lower cost than the trapped reliance-interest cohorts face; institutional power to set the exit terms. The constraint appears as coordination achievement — proving the doctrine.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_restored_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADE AND REGULATORY COALITION (SCAFFOLD) — States and firms organizing post-exit regulatory alignment see the constraint as temporary: new bilateral/plurilateral agreements (CPTPP accession, mutual recognition arrangements, regulatory equivalence arrangements) are building alternative coordination pathways. The extraction mechanism (trading reliance-interest costs for sovereigntyrestoration) is sunset by institutional innovation — regulatory recognition treaties reduce the gap between supranational harmonization and independent national standard-setting. Constrained but with visible exit pathway: sunset timeline ~10-15 years for regulatory alignment infrastructure to mature.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_restored_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SUPRANATIONAL INSTITUTIONS (PITON) — The EU legal order, predicated on irreversibility and supremacy, encounters the sovereignty-restored reading as a refutation of its foundational architecture. Yet the supranational system persists — reinvigorated among member states committed to deepening, rule-of-law enforcement mechanisms activated, institutional legitimacy appeals restructured around 'voluntary membership' framing. The exit was structurally possible all along, the doctrine claims, so the institutions' degradation is performative: they continue to operate as if irreversible while acknowledging revocability in theory. Theater ratio ≥0.70: supremacy narratives maintained despite demonstrated exit capacity. Arbitrage: can shift to looser confederal arrangements or core vs periphery tiers, preserving institutional forms.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_restored_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal frame, constitutional sovereignty and supranational supremacy are logically incommensurable: no legal system can truly be both supreme and subordinate simultaneously; therefore, membership must have necessarily hollowed Parliament's sovereignty, and exit restoration simply revealed what was always true. Sovereignty is treated as a conserved quantity in legal physics — zero-sum between Parliament and supranational order. However, the structural data contradicts this mountain classification. The doctrine (pooled reading via the balanced constitution) held that both could coexist as long as Parliament could revoke. Exit proves revocation possible, but that does not settle whether subordination was real during membership. The mountain perspective naturalizes a political choice (sovereignty restoration) as logical necessity. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_restored_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_membership_and_exit__sovereignty_restored_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_restored_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_restored_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_membership_and_exit__sovereignty_restored_reading, TR),
    TR >= 0.70.

:- end_tests(eu_membership_and_exit__sovereignty_restored_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The reading asserts that exit proved Parliament's retained capacity to revoke pooled supremacy — but the assertion only proves revocability, not that sovereignty was never hollowed during membership. The extractiveness value reflects the asymmetry: reliance-interest cohorts bear transition costs (economic disruption, mobility loss, regulatory discontinuity) while the take-back-control coalition captures political gains (narrative victory, legislative scope recovery, electoral vindication). The extraction is not maximal (snare-level ≥0.46) because (a) the reliance interests were willing participants in the supranational order, gaining coordination benefits; (b) the exit was achieved through democratic referendum, giving all voters equal formal say; (c) regulatory alignment scaffolding (CPTPP, bilateral arrangements) offers pathways to recover some coordination benefits. But it is real and substantial: the costs are concentrated on those most dependent on supranational frameworks, while benefits are distributed across a political coalition. Suppression (0.48): Moderate-high. Multiple mechanisms suppress alternative interpretations: (1) the referendum frame (one-off binary choice) suppresses ongoing cost-benefit deliberation; (2) the 'will of the people' rhetoric suppresses argument about distributional asymmetry; (3) the doctrine narrative ('Parliament proved what it always had') suppresses examination of whether substantive subordination was real during membership; (4) the institutional-transition costs suppress reliance-interest organization (firms focused on survival cannot organize political counter-coalition). Yet suppression is not total (snare-level ≥0.60): courts retained review power (R (Miller) v Secretary of State upheld parliamentary primacy); devolved governments and reliance-interest groups organized opposition; EU law integration was reversible through democratic process (no coup, no secret supremacy). Theater ratio (0.55): Moderate-high. Parliamentary sovereignty restoration claims involve significant performative content. The reading must rhetorically assert that Parliament's sovereignty 'was never truly lost' while simultaneously demonstrating that it had to be recovered through exit. This performative work involves: (1) reinterpreting supremacy-as-choice narrative (pooled reading); (2) managing institutional performance of parliamentary supremacy despite administrative reliance on EU rule frameworks during membership; (3) performing democratic vindication of 'taking back control' narrative against counterclaim that exit imposed costs on vulnerable cohorts. The theater rises over the interval as the reading must do increasing institutional work to maintain the sovereignty-never-lost claim while managing the practical recovery process.
 *
 * PERSPECTIVAL GAP:
 *   The reading exhibits the full perspectival range. The take-back-control coalition (institutional/arbitrage) sees pure coordination restoration — Parliament proved it could revoke supremacy and did, validating the pooled-choice doctrine. Reliance-interest holders (powerless/trapped) see pure extraction — they bear all costs of transition and have no capacity to exit the extraction itself. The integrationist settlement (institutional/constrained) sees tangled rope — the coordination function of harmonized rules is lost, but the settlement must absorb redesign costs with limited unilateral agency. The international trade coalition (organized/constrained) sees scaffold — regulatory alignment provides a sunset pathway; ~10-15 years of transition costs are time-limited. The supranational institutions (institutional/arbitrage) see degradation (piton) — their foundational irreversibility claim is falsified, yet they persist through reinvigorated integration and revised legitimacy frames, maintaining performative supremacy narratives despite acknowledged revocability. The analytical observer (analytical/analytical) risks seeing zero-sum constitutional physics (mountain) — sovereignty is a conserved quantity; exit necessarily proves it was lost during membership. But this naturalizes a political interpretation (sovereignty-lost reading's claim) as logical necessity. The structural data reveals it as one reading among three, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality and effective extractiveness (chi) from the agent's structural position: power level, exit options, beneficiary/victim status. The take-back-control coalition (institutional power, arbitrage exit) experiences low d — they are net beneficiaries with high exit capacity. Reliance-interest holders (powerless, trapped) experience high d — they are victims with no meaningful exit. The integrationist settlement (institutional power, constrained exit) experiences moderate d — they lose coordination function but retain some institutional capacity to negotiate transition terms. The international trade coalition (organized, constrained) experiences moderate d — they are partially trapped in regulatory redesign but can organize collective action for bilateral arrangements. Each derives its classification from this d value plus ε and the scope modifiers (regional scope for constitutional actors, national for parliamentary, global for trade). The perspectival gap reveals the reading's asymmetry: beneficiaries and institutional actors see coordination or restoration; victims see extraction or loss.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_thesis_empirical_status,
    'Was EU membership genuinely irreversible in constitutional law and political structure, or was the irreversibility narrative a contingent institutional claim that proved refutable?',
    'Historical analysis of pre-exit constitutional doctrine in member states; comparison of formal mechanisms (Treaty amendment unanimity, Article 50 insertion timing) vs. practical political barriers (coalition costs, reliance interest organization); counterfactual analysis of pre-2016 exit feasibility',
    'If genuinely irreversible: sovereignty was hollowed; exit restoration was revolutionary break. If contingently presented as irreversible: the pooled reading holds; exit proves doctrine empirically but does not resolve the constitutional reality during membership.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irreversibility_thesis_empirical_status, empirical, 'Whether EU membership was constitutionally irreversible or contingently presented as such').

omega_variable(
    subordination_vs_coordination_during_membership,
    'During membership, did supremacy of EU law function as constitutional subordination of Parliament or as Parliament''s continuing choice within a coordination framework?',
    'Legal doctrine analysis of how supremacy was framed and justified in each member state''s constitutional tradition; empirical analysis of whether Parliament retained de facto gate-control over EU law incorporation; examination of whether supremacy operated symmetrically across member states or asymmetrically favoring core states',
    'If subordination: sovereignty-lost reading is structural reality; exit proves doctrine but reverses only the form, not the substance of power loss. If coordination: sovereignty-pooled reading holds; exit outcome depends on post-exit regulatory alignment costs, not on recovering lost sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_vs_coordination_during_membership, conceptual, 'Whether supremacy operated as constitutional subordination or continuing choice').

omega_variable(
    reliance_interest_extraction_measurement,
    'What magnitude of costs borne by reliance-interest holders (workers, firms, citizens with cross-border life projects) constitutes extractiveness under this reading, and how does that extractiveness compare to the benefits of sovereignty restoration?',
    'Quantification of transition costs (job displacement, business continuity disruption, citizen mobility loss, regulatory compliance burden); comparison to demonstrable parliamentary gains in legislative scope recovery; survey data on whether reliance-interest holders perceive the trade-off as fair extraction or legitimate political cost',
    'If costs are high relative to gains: snare classification from reliance perspective is dominant; tangled_rope understates the asymmetry. If costs are moderate and time-limited by regulatory alignment scaffolding: tangled_rope and scaffold classifications hold; extraction is real but bounded. Measurement directly affects whether mandatrophy is satisfied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliance_interest_extraction_measurement, empirical, 'Magnitude of reliance-interest costs relative to sovereignty restoration benefits').

omega_variable(
    pool_revocability_as_committer_reading_ambiguity,
    'This reading (sovereignty restored) instantiates the claim that pooling was always revocable (core of the pooled reading''s doctrine). Does that mean this reading endorses the pooled reading''s constitutional claim, or does exit proving revocability settle a factual question while leaving the constitutional framing contested?',
    'Examine the kernel: the pooled reading''s axiom was ''supremacy was Parliament''s continuing choice, therefore revocable at will and sovereignty never truly lost.'' Exit proves revocability. But the sovereignty-lost reading held ''revocable in theory, but subordination in fact.'' The sovereignty-restored reading says ''exit proved the doctrine''—proving revocability. This proves the pooled reading''s factual claim (revocability is real) but not its constitutional interpretation (whether subordination was nonetheless real during membership). The reading_relation to pooled reading should be ''influences'' (exit proof shifts empirical ground) not ''forecloses'' (exit proof does not resolve the conceptual constitution question).',
    'If foreclosing: the readings collapse into one; the kernel contest is semantic, not structural. If influences: the readings remain live; they agree on exit mechanics but differ on what membership meant, and exit does not settle the meaning question retroactively.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pool_revocability_as_committer_reading_ambiguity, conceptual, 'Whether exit proof of revocability resolves or merely influences the constitutional meaning of membership').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_membership_and_exit__sovereignty_restored_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eumex_theater_t0_membership_stable, eu_membership_and_exit__sovereignty_restored_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(eumex_theater_t3_referendum_discourse, eu_membership_and_exit__sovereignty_restored_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(eumex_theater_t6_institutional_performance, eu_membership_and_exit__sovereignty_restored_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(eumex_extractiveness_t0_membership_stable, eu_membership_and_exit__sovereignty_restored_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(eumex_extractiveness_t3_referendum_period, eu_membership_and_exit__sovereignty_restored_reading, base_extractiveness, 3, 0.31).
narrative_ontology:measurement(eumex_extractiveness_t6_withdrawal_acts, eu_membership_and_exit__sovereignty_restored_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eumex_suppression_t0_legal_order_stable, eu_membership_and_exit__sovereignty_restored_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(eumex_suppression_t3_referendum_exit_costs, eu_membership_and_exit__sovereignty_restored_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(eumex_suppression_t6_transition_barriers, eu_membership_and_exit__sovereignty_restored_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_membership_and_exit__sovereignty_restored_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_membership_and_exit__sovereignty_restored_reading, eu_membership_and_exit__sovereignty_lost_reading).
narrative_ontology:affects_constraint(eu_membership_and_exit__sovereignty_restored_reading, eu_membership_and_exit__sovereignty_pooled_reading).

% DUAL FORMULATION NOTE:
% The eu_membership_and_exit kernel decomposes into three constraint stories, one per reading. Each reading has a different ε value, different beneficiary/victim declarations, and different terminal classification. The sovereignty_restored_reading (this story) has ε=0.38 (Tangled Rope); the sovereignty_lost_reading has higher ε reflecting maximal subordination; the sovereignty_pooled_reading has lower ε reflecting coordination function throughout. All three are linked via network.affects_constraints because they are readings of the same kernel and compete for constitutional authority. The reading_relations in cs_structure specify how each reading relates to its siblings structurally (forecloses, influences, coexists_with).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_membership_and_exit__sovereignty_restored_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
