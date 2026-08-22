% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at as Mutual Obligation: Pharaonic Reciprocity Norm
 *   domain: religious/political/economic
 *
 * SUMMARY:
 *   This story instantiates the RECIPROCITY reading of the Ma'at order
 *   principle: Ma'at is understood as imposing genuine, mutual, and
 *   enforceable obligations on Pharaoh — justice, flood/famine management,
 *   and proportionate redistribution — in exchange for tribute, labor, and
 *   obedience. Under this reading, Pharaoh is subject to the norm rather than
 *   identical with it; visible failure (famine mismanagement, unjust rulings,
 *   collapse of central storage) delegitimizes the throne and licenses
 *   withdrawal of cooperation, as the First Intermediate Period laments and
 *   tomb-autobiography record attest. This is distinct from the
 *   divine_mandate_reading, where the ruler embodies Ma'at and cannot violate
 *   it by definition (no reciprocity check exists), and from the
 *   distributed_maintenance_reading, where the obligation is diffused across
 *   all social stations rather than concentrated as a bilateral exchange with
 *   the crown. The three readings are separate constraints sharing a kernel;
 *   this file addresses only the bilateral-exchange claim.
 *
 * KEY AGENTS:
 *   - pharaoh_and_royal_household: agenda_setter/beneficiary (institutional/arbitrage) — administers and profits from the exchange, answerable to the norm only under visible failure
 *   - temple_priesthood: beneficiary/agenda_setter (institutional/arbitrage) — controls the interpretive apparatus certifying compliance
 *   - provincial_nomarchs: beneficiary/payer (powerful/constrained) — intermediate administrators with mixed incentives
 *   - peasant_cultivators: payer (powerless/trapped) — bears the material cost, gains famine/flood protection and adjudication
 *   - corvee_laborers: payer (powerless/trapped) — bears bounded, seasonal extraction under the norm's limiting logic
 *   - royal_scribes_and_officials: observer/beneficiary (organized/constrained) — record-keepers whose documentation constitutes the norm's evidentiary trace
 *   - future_historians_and_priests: excluded (analytical) — retrospective judges with no contemporaneous voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.42).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.48).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at as Mutual Obligation: Pharaonic Reciprocity Norm").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "religious/political/economic").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, '795502c6-156f-4769-9e97-4b2312044f7f').
narrative_ontology:cs_kernel_codification('795502c6-156f-4769-9e97-4b2312044f7f', distributed).
narrative_ontology:cs_authority_grounding('795502c6-156f-4769-9e97-4b2312044f7f', lineage).
narrative_ontology:cs_interpretation_layer_present('795502c6-156f-4769-9e97-4b2312044f7f').
narrative_ontology:cs_reading_relation('795502c6-156f-4769-9e97-4b2312044f7f', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('795502c6-156f-4769-9e97-4b2312044f7f', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('795502c6-156f-4769-9e97-4b2312044f7f', foundational, pharaoh_bound_by_breachable_obligation).
narrative_ontology:cs_axiom_status(pharaoh_bound_by_breachable_obligation, holdable).
narrative_ontology:cs_axiom_grounding('795502c6-156f-4769-9e97-4b2312044f7f', pharaoh_bound_by_breachable_obligation, conventional).
narrative_ontology:cs_axiom('795502c6-156f-4769-9e97-4b2312044f7f', foundational, failed_obligation_licenses_withdrawal_of_support).
narrative_ontology:cs_axiom_status(failed_obligation_licenses_withdrawal_of_support, holdable).
narrative_ontology:cs_axiom_grounding('795502c6-156f-4769-9e97-4b2312044f7f', failed_obligation_licenses_withdrawal_of_support, instrumental).
narrative_ontology:cs_reference_frame('795502c6-156f-4769-9e97-4b2312044f7f', old_kingdom_bilateral_covenant).
narrative_ontology:cs_drift_state('795502c6-156f-4769-9e97-4b2312044f7f', first_intermediate_period_collapse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('795502c6-156f-4769-9e97-4b2312044f7f', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh_and_royal_household).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, provincial_nomarchs).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, peasant_cultivators).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, corvee_laborers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, royal_scribes_and_officials).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, provincial_nomarchs).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, reciprocal_kingship_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, cosmic_balance_maintained_through_exchange).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the granaries, courts, and irrigation works that constitute the visible discharge of Ma'at's obligations, and in turn collects tribute, corvee labor, and religious legitimacy from the arrangement. Can adjust the terms of distribution unilaterally in ordinary times; is answerable to the reciprocity norm only when failure becomes visible enough (famine, military collapse, chronic injustice) to generate organized non-cooperation.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh_and_royal_household, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, pharaoh_and_royal_household, beneficiary).

% Administers the rituals that certify whether Ma'at is being upheld and controls substantial temple land and grain reserves granted under the reciprocity logic. Benefits from being the interpretive authority on whether Pharaoh has met his obligations, and can withdraw ritual endorsement as leverage.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, temple_priesthood, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, temple_priesthood, agenda_setter).

% Administer redistribution at the regional level and take a share for local administration and personal enrichment. Depend on the central reciprocity arrangement for legitimacy but can exploit weak central oversight to divert resources; also bear cost when central failure forces them to cover shortfalls locally.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, provincial_nomarchs, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, provincial_nomarchs, payer).

% Surrender grain, labor, and produce as the material substance of the reciprocity exchange, receiving flood-control, granary reserves against famine, and dispute adjudication in return. Have no realistic exit from the land or the arrangement, but the reciprocity norm gives their suffering (visible famine, unjust rulings) rhetorical and sometimes practical weight — chronicled failures of Ma'at are the language in which their grievances become politically legible.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, peasant_cultivators, payer,
    powerless, biographical, trapped, local).

% Conscripted seasonally for state construction and irrigation projects justified as maintaining the stability Ma'at requires. Cannot refuse without punishment, but the obligation is bounded by norm (seasonal, compensated in kind, not indefinite) rather than open-ended — the reciprocity frame is what keeps the levy from becoming permanent bondage.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, corvee_laborers, payer,
    powerless, immediate, trapped, local).

% Record harvests, disputes, and famine years, producing the textual record (tomb autobiographies, wisdom literature, the Eloquent Peasant tradition) by which failures of royal obligation are named and remembered. Benefit from administrative office but their professional function depends on the reciprocity norm being real enough to require documentation and judgment.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, royal_scribes_and_officials, observer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, royal_scribes_and_officials, beneficiary).

% Later generations who judge a reign's legitimacy retrospectively through chronicles and monument inscriptions, applying the reciprocity standard to verdicts on collapsed or failed reigns (the First Intermediate Period laments). Not present to negotiate terms in the moment; their judgment operates only after the fact, as a delayed and indirect check.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, future_historians_and_priests, excluded,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a real, mutually-binding basis for large-scale flood-control, granary storage against famine, and centralized dispute adjudication — problems that require coordination beyond what any single village or noble estate can solve alone, distributed through a Pharaoh who is himself bound by the terms.
% TRANSFER_FUNCTION: Moves grain, corvee labor, and tribute from peasant cultivators and provincial producers upward to the royal and temple administration, and moves flood management, famine-reserve access, and legal adjudication back down — the reciprocity reading holds that the downward flow is a genuine, norm-bound obligation, not a gift, and its failure is itself extractive.
% ABSENT_VOICES: Peasant cultivators and corvee laborers have no seat in defining what counts as adequate distribution or justice — the standard is set and interpreted by priesthood and royal administration. Their objections surface only indirectly, through literary genres like the wisdom laments and Eloquent Peasant tradition, and through withdrawal of cooperation (banditry, migration, non-compliance) during periods of visible famine or injustice.
% DISAPPEARANCE_RATIONALE: If the reciprocity norm vanished — if Pharaoh's legitimacy were no longer conditioned on providing justice and stability in exchange for resources and labor — the extraction from cultivators would lose its only check; nomarchs and priests would lose their interpretive leverage over the throne; and the ideological basis for organized resistance during famine or misrule (as in the First Intermediate Period collapse narratives) would disappear, removing the main non-violent mechanism by which failed obligation could be named and acted upon.
% FOUNDING_PROBLEM: Nile flood variability and the scale of irrigation, granary, and defense infrastructure required to survive it exceed what any village or local elite could organize alone; a centralized authority was needed to coordinate resource pooling and redistribution across an entire river valley, but that authority needed a legitimating norm that also bounded what it could take.
% FOUNDING_PROBLEM_CORROBORATION: Temple and royal inscriptions attest the founding problem as permanently live (cosmic order requires constant renewal). Independent of the benefiting parties, the First Intermediate Period wisdom literature and tomb autobiographies of provincial officials describe periods when royal failure to distribute and adjudicate produced open unrest and the collapse of central authority — corroborating from outside the throne's own self-presentation that the reciprocity obligation was treated as real and enforceable by contemporaries, not merely rhetorical.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate ceiling (0.42 by interval end) reflecting the reciprocity reading's own structural claim: the norm bounds what the crown may take by tying legitimacy to genuine service delivery, so extraction cannot rise indefinitely without triggering the delegitimization mechanism the norm itself provides. This is lower than would be authored for the divine_mandate_reading (where no such ceiling exists structurally) and comparably positioned relative to the distributed_maintenance_reading (which diffuses rather than concentrates the extraction target). Suppression sits at a middle value (0.48) because compliance depends partly on ideological internalization (belief that resisting Pharaoh disrupts cosmic order) and partly on coercive capacity (corvee enforcement, tax collection); theater_ratio rises at t=240 to model a period of documented ritual intensification without matching material delivery (a plausible reading of the run-up to the First Intermediate Period collapse), then partially recovers as later dynasties re-couple ritual claims to actual redistribution.
 *
 * PERSPECTIVAL GAP:
 *   From Pharaoh's and the priesthood's seats, the arrangement is functioning reciprocity — a real exchange they administer in good faith, with occasional lapses. From the peasant and corvee seats, the same structure is experienced as a one-directional levy whose only check is catastrophic and rare (open unrest, migration, dynastic collapse) rather than routine and reliable. The engine should compute these divergently from the stakeholder power/exit data; the reciprocity reading does not claim the check operates smoothly, only that it structurally exists and periodically binds.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh and the priesthood are declared beneficiaries with arbitrage-grade exit (they set and can adjust the terms) — low derived directionality. Peasant cultivators and corvee laborers are declared victims with trapped exit — high derived directionality, appropriately amplified given their local spatial scope and immediate/biographical time horizons. Nomarchs and scribes are dual-positioned (secondary roles) because they administer the extraction locally while also bearing some of its risk and legitimacy burden — this asymmetry is why they carry both beneficiary and payer/observer roles rather than a single clean role.
 *
 * MANDATROPHY ANALYSIS:
 *   The reciprocity reading is precisely constructed to prevent the mislabeling risk in either direction: it refuses to treat Ma'at as pure extraction (which would erase the genuine flood-control and famine-reserve coordination function attested by granary archaeology and administrative records) and refuses to treat it as pure legitimate coordination (which would erase the peasant/corvee cost and the documented periods, like the First Intermediate Period, when the obligation was understood by contemporaries to have been broken). The founding_problem_status is authored as contested rather than dead or live because the coordination need (Nile-scale flood and famine management) remained genuinely live throughout, while whether Pharaoh's OBLIGATION under it was being honored fluctuated by reign — exactly the reciprocity reading's structural claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_embodiment_framing,
    'Did contemporary Egyptians (beyond court ideology) actually hold Pharaoh accountable to a bilateral, breachable standard, or is the reciprocity reading a retrospective/literary construction read back from crisis-period texts (First Intermediate Period laments) onto periods of stability?',
    'Comparative textual analysis across dynasties: are reciprocity-style accountability claims present in stable-period administrative and legal texts, or do they appear predominantly in post-collapse literature justifying the collapse retroactively?',
    'If reciprocity language is predominantly a crisis-period retrospective construction, this reading''s claim to describe an operative, standing constraint (rather than an ex-post narrative device) weakens substantially, pushing the honest classification toward something closer to the divine_mandate_reading''s operative reality with reciprocity as legitimating theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_embodiment_framing, conceptual, 'Whether the reciprocity obligation was a standing operative constraint or a retrospective crisis narrative.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the correct framing of Ma''at a single bilateral exchange between Pharaoh and subjects (this reading), a top-down mandate with no true reciprocity (divine_mandate_reading), or a diffuse multi-actor maintenance obligation (distributed_maintenance_reading) — and which framing the surviving textual record best supports may itself vary by genre (royal inscription vs. wisdom literature vs. tomb autobiography).',
    'Cross-genre corpus analysis of Ma''at references across royal, priestly, and private/administrative textual registers, tagged by the framing each register implies.',
    'If royal inscriptions overwhelmingly support divine_mandate framing while wisdom literature and tomb autobiographies support reciprocity framing, the three readings may map onto genre-specific ideological positions rather than competing historical claims about a single institution — meaning all three constraints are real but describe different social layers'' beliefs about the same kernel simultaneously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three kernel readings track genuine competing claims or genre-differentiated ideological registers coexisting at one time.').

omega_variable(
    extraction_ceiling_enforceability,
    'How was the reciprocity norm''s ''moderate extraction ceiling'' actually enforced in practice — through elite coordination (nomarch/priest withdrawal of support), mass action (peasant unrest, flight, banditry), or purely ex-post narrative delegitimization with no contemporaneous enforcement mechanism at all?',
    'Archaeological and administrative record correlation: tax/grain-distribution records against known famine years and periods of attested unrest or nomarch assertions of autonomy.',
    'If enforcement was purely narrative/retrospective, the effective suppression this reading authors is too low — the mechanism protecting cultivators was weaker than the reciprocity story suggests, and the constraint tilts closer to tangled_rope-with-thin-check or even snare in practice despite the reciprocity framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_ceiling_enforceability, empirical, 'What mechanism, if any, actually enforced the reciprocity ceiling in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__reciprocity_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(maat_tr_t160, maat_order_principle__reciprocity_reading, theater_ratio, 160, 0.35).
narrative_ontology:measurement(maat_tr_t240, maat_order_principle__reciprocity_reading, theater_ratio, 240, 0.5).
narrative_ontology:measurement(maat_tr_t320, maat_order_principle__reciprocity_reading, theater_ratio, 320, 0.38).
narrative_ontology:measurement(maat_tr_t400, maat_order_principle__reciprocity_reading, theater_ratio, 400, 0.4).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__reciprocity_reading, base_extractiveness, 80, 0.33).
narrative_ontology:measurement(maat_be_t160, maat_order_principle__reciprocity_reading, base_extractiveness, 160, 0.38).
narrative_ontology:measurement(maat_be_t240, maat_order_principle__reciprocity_reading, base_extractiveness, 240, 0.45).
narrative_ontology:measurement(maat_be_t320, maat_order_principle__reciprocity_reading, base_extractiveness, 320, 0.4).
narrative_ontology:measurement(maat_be_t400, maat_order_principle__reciprocity_reading, base_extractiveness, 400, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__reciprocity_reading, suppression_requirement, 80, 0.35).
narrative_ontology:measurement(maat_su_t160, maat_order_principle__reciprocity_reading, suppression_requirement, 160, 0.42).
narrative_ontology:measurement(maat_su_t240, maat_order_principle__reciprocity_reading, suppression_requirement, 240, 0.55).
narrative_ontology:measurement(maat_su_t320, maat_order_principle__reciprocity_reading, suppression_requirement, 320, 0.45).
narrative_ontology:measurement(maat_su_t400, maat_order_principle__reciprocity_reading, suppression_requirement, 400, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(maat_order_principle__reciprocity_reading, 0.15).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the maat_order_principle kernel. divine_mandate_reading claims Pharaoh cannot violate Ma'at by definition (no reciprocity check, no bilateral obligation) and would author near-zero suppression-check and a much higher, effectively unbounded extraction ceiling. distributed_maintenance_reading diffuses the maintenance obligation across all social stations rather than concentrating it as a bilateral crown/subject exchange, changing both the beneficiary/victim structure and the coordination_type framing. All three share the same underlying textual kernel (Ma'at) but diverge on WHO is bound, WHETHER breach is possible, and WHERE the extraction ceiling sits — exactly the structural delta the ε-invariance principle requires resolving via decomposition rather than a single averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
