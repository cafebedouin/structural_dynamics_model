% ============================================================================
% CONSTRAINT STORY: article_370_special_status__temporary_provision_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_370_temporary_provision, []).

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
 *   constraint_id: article_370_special_status__temporary_provision_reading
 *   human_readable: Article 370 Temporary Provision Reading: Special Status as Transitory Integration Mechanism
 *   domain: constitutional_law/federal_compact
 *
 * SUMMARY:
 *   Article 370 of the Indian Constitution grants Jammu and Kashmir special
 *   status — a distinct legislative authority, separate law enforcement, and
 *   autonomy in internal affairs. The constraint story examines ONE reading
 *   of Article 370's legitimacy: the temporary provision reading, which
 *   interprets the text's heading ('Temporary, Transition and Special
 *   Provisions') as designating a constitutional arrangement always intended
 *   as transitional, pending eventual full integration into the ordinary
 *   constitutional order. This reading suppresses permanent-asymmetry claims
 *   by appeal to the text's own temporality label. The beneficiary is the
 *   union's integration doctrine — the reading affirms that integration is
 *   constitutionally scheduled, not contingent. The victim set includes
 *   autonomy expectations built on the assumption that special status could
 *   be permanent or renegotiable. The extractiveness reflects that the
 *   temporary designation constrains Kashmir's ability to claim
 *   constitutional permanence while the union retains the initiative to
 *   define 'integration.' The constraint exhibits classic tangled_rope
 *   signature: genuine coordination function (the provision enables accession
 *   by framing autonomy as temporary, solving the political problem of union
 *   expansion), asymmetric extraction (the union benefits from scheduled
 *   integration; Kashmir bears the cost of temporality), and active
 *   enforcement (the temporary label must be continuously invoked to suppress
 *   permanence claims). The measurements trace increasing extractiveness and
 *   theater over 69 years as the provision's 'temporary' character persists
 *   without integration occurring — the longer the provision remains without
 *   sunset, the more its temporality becomes performative. The theater ratio
 *   rises from 0.32 (temporality seems credible at accession in 1950) to 0.48
 *   (temporality is clearly inert by 2019, the label functions without
 *   effect). The suppression requirement rises as Kashmir's autonomy
 *   movements increasingly challenge the temporary framing, requiring more
 *   active enforcement to maintain the constraint's legitimacy.
 *
 * KEY AGENTS:
 *   - Jammu and Kashmir (historical state): Primary victim (powerless/trapped) — locked into the temporary provision's frame at accession; cannot exit the integration schedule or claim constitutional permanence
 *   - Kashmir autonomy movements and political stakeholders: Secondary victim (moderate/constrained) — constrained by the temporary designation yet also benefiting from special status provisions during the transitional period
 *   - The Union (India's central authority): Primary beneficiary (institutional/arbitrage) — benefits from the provision's implicit integration schedule; retains constitutional authority to define and enforce integration
 *   - Constitutional integration doctrine: Beneficiary (institutional) — the legal framework treating integration as constitutionally inevitable
 *   - The 2019 abrogation process: Secondary actor (institutional/analytical) — tested the temporary provision by attempting actual termination via presidential order
 *   - Analytical observer at civilizational scope: Observer position (analytical/analytical) — risks naturalizing contingent constitutional arrangements as inevitable union destiny
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_370_special_status__temporary_provision_reading, 0.38).
domain_priors:suppression_score(article_370_special_status__temporary_provision_reading, 0.52).
domain_priors:theater_ratio(article_370_special_status__temporary_provision_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_370_special_status__temporary_provision_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_370_special_status__temporary_provision_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(article_370_special_status__temporary_provision_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_370_special_status__temporary_provision_reading, tangled_rope).
narrative_ontology:human_readable(article_370_special_status__temporary_provision_reading, "Article 370 Temporary Provision Reading: Special Status as Transitory Integration Mechanism").
narrative_ontology:topic_domain(article_370_special_status__temporary_provision_reading, "constitutional_law/federal_compact").

domain_priors:requires_active_enforcement(article_370_special_status__temporary_provision_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_370_special_status__temporary_provision_reading, '1f0b031d-c3d3-4b66-9c87-8374d590bfcf').
narrative_ontology:cs_kernel_codification('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', fixed_text).
narrative_ontology:cs_authority_grounding('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', lineage).
narrative_ontology:cs_interpretation_layer_present('1f0b031d-c3d3-4b66-9c87-8374d590bfcf').
narrative_ontology:cs_reading_relation('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', article_370_special_status__abrogation_2019_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', article_370_special_status__compact_federalism_reading, forecloses).
narrative_ontology:cs_axiom('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', foundational, temporality_designates_union_integration_authority).
narrative_ontology:cs_axiom_status(temporality_designates_union_integration_authority, holdable).
narrative_ontology:cs_axiom_grounding('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', temporality_designates_union_integration_authority, deontological).
narrative_ontology:cs_axiom('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', foundational, constitutional_supremacy_overrides_compact_conditions).
narrative_ontology:cs_axiom_status(constitutional_supremacy_overrides_compact_conditions, holdable).
narrative_ontology:cs_axiom_grounding('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', constitutional_supremacy_overrides_compact_conditions, deontological).
narrative_ontology:cs_reference_frame('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', constitutional_integration_design).
narrative_ontology:cs_drift_state('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', pre_abrogation_2019, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1f0b031d-c3d3-4b66-9c87-8374d590bfcf', '').
narrative_ontology:cs_kernel_id(article_370_special_status__temporary_provision_reading, article_370_special_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_370_special_status__temporary_provision_reading, union_integration_doctrine).
narrative_ontology:constraint_victim(article_370_special_status__temporary_provision_reading, jammu_kashmir_autonomy_expectations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KASHMIR CONSTITUENT ASSEMBLY (SNARE) — Locked into the temporary provision's frame at the moment of accession; cannot renegotiate or exit the integration schedule. The assembly that ratified accession had its autonomy defined as temporary by the constitutional text itself. Extraction flows from the state's inability to claim permanence — the special status's own label (temporary) delegitimizes permanence claims before they can be made.
constraint_indexing:constraint_classification(article_370_special_status__temporary_provision_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: KASHMIRI POLITICAL STAKEHOLDERS (TANGLED ROPE) — Constrained by both the temporary label and the absence of renegotiation mechanisms. But also benefit from the special constitutional status during the transitional period — separate legislative authority, distinct law enforcement, property protections. The constraint coordinates autonomy provisioning while extracting commitment to eventual full integration. Real agency blocked by the provision's self-canceling design.
constraint_indexing:constraint_classification(article_370_special_status__temporary_provision_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNION INTEGRATION PROJECT (ROPE) — Sees Article 370 as a coordination mechanism: the temporary provision solves the accession problem by stating upfront that special status is transitional. Integration can proceed as designed without renegotiation. The beneficiary of the constraint's temporal structure — the union's constitutional authority to integrate proceeds by the provision's own schedule. Net beneficiary.
constraint_indexing:constraint_classification(article_370_special_status__temporary_provision_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized actors (civil society, reform coalitions) see Article 370 as a sunset structure ready for principled renegotiation. The temporary label offers a transition point where stakeholders could deliberate on integration terms rather than merely accept integration as fait accompli. Low effective extraction because the organized view retains agency over the renegotiation moment. Sunset rationale: Article 370's own frame designates it as temporary — sunset is built into the kernel itself.
constraint_indexing:constraint_classification(article_370_special_status__temporary_provision_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMALIST CONSTITUTIONAL COMMENTARY (PITON) — Constitutional scholars treating 'temporary' as a performative label: the provision has been in place for 70 years yet its temporary status persists without revision mechanism or sunset trigger. The label functions as ritual — marking the provision as provisional in form while allowing indefinite continuation in practice. The formalist interpretation maintains the provision's constitutional fiction without structural effect. Theater-high because the 'temporary' label generates no actual integration timeline.
constraint_indexing:constraint_classification(article_370_special_status__temporary_provision_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From the broadest constitutional perspective, Article 370's temporary designation reflects a fundamental principle: all constitutional arrangements are contingent on their continued legitimacy, and constitutional supremacy requires that no provision can claim immunity from revision by the supreme constitutional authority. The 'temporary' label naturalizes integration as an inevitable constitutional consequence. Engine will flag this as a false summit: the inevitability claim rests on beneficiary interests, not on constitutional necessity.
constraint_indexing:constraint_classification(article_370_special_status__temporary_provision_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_370_special_status__temporary_provision_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_370_special_status__temporary_provision_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_370_special_status__temporary_provision_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_370_special_status__temporary_provision_reading, TR),
    TR >= 0.70.

:- end_tests(article_370_special_status__temporary_provision_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The temporary provision imposes a real constraint on Kashmir's autonomy claims — the text's label suppresses arguments for permanent special status. But the extractiveness is not maximal because the provision also provides genuine coordination benefits: accession becomes politically feasible by offering time-bounded autonomy. The union benefits from this arrangement, but not through crude coercion — through constitutional scheduling. The rising trajectory from 0.15 (accession 1950, temporality fresh) to 0.38 (pre-abrogation 2019) reflects accumulating extraction as integration fails to occur despite the temporary designation. Theater ratio (0.48): Moderate-high. The 'temporary' label has evolved from credible constraint (1950s: integration seemed plausible within a generation) to performative framing (2019: 70 years have passed, no integration has occurred, yet the label persists). The theater ratio rises as the label's inertia becomes apparent — it continues to suppress permanence claims despite failing to trigger any actual integration. Suppression (0.52): Moderate-high. The temporary designation actively suppresses Kashmir's ability to claim constitutional permanence, ground renegotiation demands, or argue for unlimited autonomy. This suppression is enforced through constitutional interpretation, political pressure, and (in 2019) direct presidential action. The suppression is not total — Kashmir retained its special constitutional status for 69 years, and autonomy movements persisted — but the constraint blocked the strongest permanence arguments.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The union integration project sees clean coordination (Rope) — the temporary label solves accession by stating design upfront. Kashmir's constituent assembly saw itself trapped (Snare) — locked into a schedule it could not renegotiate. Contemporary Kashmiri political stakeholders see mixed extraction and coordination (Tangled Rope) — the special status provided real governance authority but with the temporality sword hanging overhead. Constitutional reformers see a sunset mechanism (Scaffold) — the 'temporary' label marks a point where deliberative renegotiation could occur. Constitutional formalists see an inert ritual (Piton) — the label generates no structural effect, merely marks the provision's provisional character indefinitely. The analytical observer at civilizational scope risks seeing integration as inevitable constitutional destiny (Mountain) — but this naturalizes what is actually a contingent institutional arrangement grounded in beneficiary interests and power asymmetries. The false summit detector flags the mountain classification: the 'inevitability' of integration is not a natural law but a reading grounded in the union's beneficiary position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the temporary provision. The Kashmir constituent assembly had no exit (trapped, d ≈ 0.95) — it could accept accession with temporary autonomy or reject union entirely, but could not renegotiate the temporary designation. Contemporary Kashmiri stakeholders face constrained exit (d ≈ 0.75) — high cost to exit the Indian union, but also constrained within it by the temporality label. The union retains arbitrage options (d ≈ 0.05-0.15) — it can wait for integration, renegotiate unilaterally, or as in 2019, abrogate the provision by executive action. The integration doctrine sees the constraint as benefiting its core mission (d ≈ 0.00) — the temporary label is designed precisely to further constitutional integration. The directionality asymmetry (high d for Kashmir, low d for the union) produces the extraction signature: the constraint flows extractive force toward the union beneficiary and away from the Kashmir victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is Article 370 a legitimately temporary provision (this reading) or a foundational compact revocable only by the named constituent assembly (compact_federalism reading) or a constitutional moment resolved by the 2019 abrogation (abrogation_2019 reading)?',
    'Historical-textual analysis: compare the text''s provenance (Constituent Assembly debates, accession instrument, constitutional structure), the amendment process (did the provision ever contemplate unilateral revocation?), and the 2019 jurisprudence (manner and authority of abrogation).',
    'If temporary reading holds: integration is constitutionally designed, constraint dissolves on schedule. If compact reading holds: renegotiation requires Kashmir constituent assembly consent (impossible after 1957). If abrogation reading holds: constraint was terminated by presidential order in 2019, rendering this story a historical artifact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of Article 370''s constitutional status is legitimate?').

omega_variable(
    temporary_without_trigger,
    'What constitutes a valid temporal trigger or termination mechanism for a ''temporary'' constitutional provision?',
    'Comparative constitutional analysis: how have other democracies treated constitutional sunset provisions? What formal mechanisms exist for transitional provisions to actually transition? Did Article 370''s drafters contemplate specific integration milestones?',
    'If temporal triggers require explicit definition: Article 370 (lacking defined triggers) cannot legitimately be treated as temporary — it is indefinite pending renegotiation. If implicit integration schedule suffices: the provision''s temporary character stands without additional machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporary_without_trigger, conceptual, 'What makes a constitutional provision''s ''temporary'' status meaningful?').

omega_variable(
    unilateral_abrogation_legitimacy,
    'Can a temporarily-designated provision be abrogated unilaterally by the union without constituent assembly renegotiation?',
    'Constitutional jurisprudence review: the 2019 abrogation case outcome and the Supreme Court''s holding on manner and authority. Does the temporary label presume union authority to terminate, or does it presume bilateral/assembly agreement?',
    'If unilateral abrogation is legitimate: this reading collapses into the abrogation_2019 reading (constraint terminated, story is historical). If bilateral/assembly consent required: compact reading gains force (autonomy was purchase price, not gift). If the court left manner undecided: constraint remains under interpretive contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_abrogation_legitimacy, empirical, 'Does the 2019 abrogation validate unilateral union authority over Article 370?').

omega_variable(
    performative_temporality,
    'Does the text''s ''temporary'' label function as genuine constitutional constraint or as performative framing that permits indefinite continuation without revision?',
    'Institutional analysis: track whether the ''temporary'' label was ever invoked to set integration timelines, resource transfers, or autonomy phase-downs. Has the label generated any actual structural change toward integration?',
    'If performative: the piton reading (degraded ritual) is correct — temporality is theater without effect. Extractiveness drops to near-zero because the label is inert. If substantive: integration schedule existed (even if implicit), suppression is real (blocking permanence claims via the label), and tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_temporality, empirical, 'Is Article 370''s ''temporary'' label a real temporal constraint or a performative label?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_370_special_status__temporary_provision_reading, 0, 69).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a370tmp_theater_accession_1950, article_370_special_status__temporary_provision_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(a370tmp_theater_mid_1975, article_370_special_status__temporary_provision_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(a370tmp_theater_turmoil_2000, article_370_special_status__temporary_provision_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(a370tmp_theater_pre_abrogation_2019, article_370_special_status__temporary_provision_reading, theater_ratio, 69, 0.48).

% Extraction over time
narrative_ontology:measurement(a370tmp_extract_accession_1950, article_370_special_status__temporary_provision_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(a370tmp_extract_mid_1975, article_370_special_status__temporary_provision_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(a370tmp_extract_turmoil_2000, article_370_special_status__temporary_provision_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(a370tmp_extract_pre_abrogation_2019, article_370_special_status__temporary_provision_reading, base_extractiveness, 69, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(a370tmp_suppress_accession_1950, article_370_special_status__temporary_provision_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(a370tmp_suppress_mid_1975, article_370_special_status__temporary_provision_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(a370tmp_suppress_turmoil_2000, article_370_special_status__temporary_provision_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(a370tmp_suppress_pre_abrogation_2019, article_370_special_status__temporary_provision_reading, suppression_requirement, 69, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_370_special_status__temporary_provision_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_370_special_status__temporary_provision_reading, article_370_special_status__compact_federalism_reading).
narrative_ontology:affects_constraint(article_370_special_status__temporary_provision_reading, article_370_special_status__abrogation_2019_reading).

% DUAL FORMULATION NOTE:
% Article 370's legitimacy involves three structurally distinct constraints: the temporary_provision_reading (this file) treats the text's 'temporary' label as constitutive of the provision's nature; the compact_federalism_reading treats the accession instrument's conditions as constitutive; the abrogation_2019_reading treats the 2019 presidential action as constitutive. Each reading generates a different constraint story with different ε values. The temporary_provision reading (ε=0.38) assumes integration could occur on schedule. The compact_federalism reading (ε≈0.55) assumes the provision is locked by bilateral agreement. The abrogation_2019 reading (ε≈0.15 post-abrogation) assumes the provision's constraint was terminated. These three stories form a constraint family linked by network.affects_constraints — the upstream reading (temporary_provision) is invoked as evidence for the abrogation_2019 reading's claim that unilateral termination was constitutionally authorized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
