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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at as Reciprocal Obligation Binding Pharaoh to Justice and Provision
 *   domain: religious/political — ancient Egyptian statecraft
 *
 * SUMMARY:
 *   This story instantiates the RECIPROCITY reading of the Ma'at kernel:
 *   cosmic order is not simply embodied by Pharaoh (the divine mandate
 *   reading) nor distributed equally across every social station (the
 *   distributed maintenance reading), but constituted as a mutual,
 *   enforceable bargain in which Pharaoh owes justice, flood-year provision,
 *   and stability in exchange for taxes, labor, and obedience. The
 *   measurement series traces a cycle: extraction and enforcement intensity
 *   rise across a period of drought/administrative strain (T20-T60, modeling
 *   something like the pressures preceding the First Intermediate Period
 *   collapse) then fall as reciprocity is renegotiated or a new dynasty
 *   re-establishes the bargain (T80-T100) — the oscillation is not noise but
 *   the mechanism itself: the reciprocity norm is precisely what allows
 *   extraction to be pushed up in good years and forces it back down when
 *   failure becomes visible and resistance mounts.
 *
 * KEY AGENTS:
 *   - pharaoh_and_royal_court: agenda_setter/beneficiary (institutional/arbitrage) — sets terms of the bargain, collects surplus and loyalty, but is structurally judgeable against the Ma'at standard
 *   - temple_priesthood: beneficiary/agenda_setter (institutional/arbitrage) — certifies royal compliance, collects independent rents
 *   - peasant_cultivators: payer (powerless/trapped) — delivers the tax/labor half of the bargain, bears the cost when the crown's half fails
 *   - corvee_laborers: payer (powerless/trapped) — conscripted labor justified by cosmic stability maintenance
 *   - modern_egyptologists: observer (analytical) — reconstructs the reciprocity claim from wisdom literature and administrative breakdown evidence
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
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at as Reciprocal Obligation Binding Pharaoh to Justice and Provision").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "religious/political — ancient Egyptian statecraft").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, 'cacddcab-a86a-4cf0-a89e-c7fc8118dcc5').
narrative_ontology:cs_kernel_codification('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', distributed).
narrative_ontology:cs_authority_grounding('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', lineage).
narrative_ontology:cs_interpretation_layer_present('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5').
narrative_ontology:cs_reading_relation('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', maat_order_principle__distributed_maintenance_reading, influences).
narrative_ontology:cs_axiom('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', foundational, pharaoh_is_subject_to_maat_not_identical_with_it).
narrative_ontology:cs_axiom_status(pharaoh_is_subject_to_maat_not_identical_with_it, holdable).
narrative_ontology:cs_axiom_grounding('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', pharaoh_is_subject_to_maat_not_identical_with_it, conventional).
narrative_ontology:cs_axiom('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', foundational, failed_obligation_licenses_withdrawal_of_support).
narrative_ontology:cs_axiom_status(failed_obligation_licenses_withdrawal_of_support, holdable).
narrative_ontology:cs_axiom_grounding('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', failed_obligation_licenses_withdrawal_of_support, instrumental).
narrative_ontology:cs_reference_frame('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', reciprocal_covenant_kingship).
narrative_ontology:cs_drift_state('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', post_old_kingdom_collapse, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('cacddcab-a86a-4cf0-a89e-c7fc8118dcc5', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh_and_royal_court).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, temple_priesthood).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, peasant_cultivators).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, corvee_laborers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, provincial_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rules by claiming to uphold Ma'at — the obligation to deliver justice, flood-driven grain distribution, and stability. In the reciprocity reading, this is a real bargain: the throne collects taxes, labor, and loyalty in exchange for administering granaries, courts, and defense. The Pharaoh sets the terms of what counts as fulfilling the obligation and controls the scribal and priestly apparatus that certifies compliance, but is understood — within this reading — to be judgeable against the standard, not identical with it.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh_and_royal_court, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, pharaoh_and_royal_court, beneficiary).

% Administers the rituals that renew Ma'at and certifies royal performance of obligation through temple economy and endowed land. Collects offerings and labor levies justified by the maintenance function; also polices the boundary of what counts as a legitimate failure of obligation, giving it leverage over both throne and populace.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, temple_priesthood, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, temple_priesthood, agenda_setter).

% Implement grain redistribution, corvee rosters, and local justice on the crown's behalf. Benefit from local authority and skimmed surplus but are themselves accountable upward for delivering the obligations Ma'at requires; a failure in their district can be read as their personal breach of reciprocity, exposing them to removal.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, provincial_officials, agenda_setter,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, provincial_officials, payer).

% Deliver grain tax and labor duty in exchange for flood-year famine relief, canal maintenance, and access to royal/temple courts for dispute resolution. In good years the reciprocity holds; in famine or maladministration years the promised return collapses while the levy often continues, and the peasant's only leverage is appeal to the reciprocity norm itself — the discourse that says a Pharaoh who fails to provide has broken the bargain and lost the claim on obedience.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, peasant_cultivators, payer,
    powerless, biographical, trapped, local).

% Conscripted for building and irrigation projects framed as maintaining cosmic-material order (dikes, temples, tombs). Cannot refuse without penalty; their consent is not sought, only their compliance, though the ideological frame promises their labor secures the stability that in turn protects them from flood failure and chaos.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, corvee_laborers, payer,
    powerless, immediate, trapped, local).

% Enforces tax collection, corvee conscription, and suppresses unrest framed as disorder (isfet) against Ma'at. Their loyalty is itself part of the reciprocal bargain — paid and provisioned by the crown in exchange for enforcing the obligations the crown owes to everyone else.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, military_and_guard_apparatus, agenda_setter,
    organized, biographical, constrained, national).

% Reconstruct the reciprocity reading from tomb inscriptions, wisdom literature (e.g. the Eloquent Peasant, Instructions to Merikare), and administrative records showing famine-era grievance language framed in terms of Pharaoh's failed duty. Debate how much this discourse constrained actual royal behavior versus functioning as post-hoc legitimation.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, modern_egyptologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__reciprocity_reading, pharaoh_and_royal_court).
narrative_ontology:fixing_cost_class(maat_order_principle__reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Frames kingship as a two-way exchange: the ruling apparatus (throne, temple, provincial administration) provides flood-year grain reserves, adjudication, and defense against external and internal disorder (isfet), in exchange for taxes, labor, and obedience. This genuinely solves a large-scale coordination problem — famine buffering and dispute resolution — that no smaller unit could solve alone.
% TRANSFER_FUNCTION: Moves grain surplus, corvee labor, and deference from cultivators and laborers upward to the throne, temple, and provincial administration; in exchange, moves back (unevenly, and contingently) famine relief, canal maintenance, and access to royal/temple justice. The reciprocity reading's distinguishing claim is that this return flow is an ENFORCEABLE OBLIGATION, not a gift — its absence is a breach, not misfortune.
% ABSENT_VOICES: Cultivators and laborers rarely appear as authors of the surviving obligation-discourse itself — the wisdom literature voicing peasant grievance (e.g. the Eloquent Peasant) was composed and copied by literate scribes for court and temple audiences, so even the 'voice of the wronged' is curated by the apparatus it indicts. Genuinely independent commoner testimony on whether the bargain was ever honored is largely unrecoverable.
% DISAPPEARANCE_RATIONALE: If the reciprocity norm vanished — if Ma'at ceased to function as a claim that could be violated — the throne would lose its principal available lever of accountability; famine-year unrest, succession disputes, and provincial defection (as seen in First and Second Intermediate Period breakdowns) show that when the obligation was widely perceived as unmet, compliance and loyalty measurably eroded. The norm's presence or absence visibly shaped whether populations continued to cooperate.
% FOUNDING_PROBLEM: Nile flood variability makes any single household's or village's grain security unreliable across years; without centralized storage, redistribution, and canal coordination, famine in a bad flood year is catastrophic. Kingship organized around Ma'at solved this by centralizing surplus and making its release a moral-cosmic obligation rather than a discretionary favor.
% FOUNDING_PROBLEM_CORROBORATION: Administrative granary records and Nilometer data from outside royal self-presentation corroborate that flood-variability famine risk was real and that redistribution infrastructure existed and functioned in many periods. However, papyri and tomb texts documenting famine, provincial revolt, and dynastic collapse (e.g. First Intermediate Period inscriptions describing feeding one's own town without royal aid) — sources not authored by the throne's court scribes but by regional nomarchs asserting their own legitimacy — corroborate that the obligation was frequently unmet, supporting the contested status rather than a clean living/dead binary.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.42 (not high) because the reciprocity reading, BY THE STORY'S OWN LOGIC, caps extraction: a Pharaoh who over-extracts without delivering justice/stability breaches the norm and risks the exact resistance/withdrawal dynamic the kernel context specifies. This is structurally different from the divine mandate reading, where no ceiling exists because the ruler cannot, by definition, violate the standard. Suppression (0.48) reflects real coercive machinery (corvee conscription, tax collection under military enforcement) but tempered by the norm's own legitimating logic, which requires visible performance of the obligation rather than naked force alone. Theater ratio (0.4) captures that a meaningful share of 'obligation-fulfillment' activity (temple ritual, royal inscriptions proclaiming justice) is performative certification layered onto real redistribution — genuine granary/canal administration exists alongside considerable ideological maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh and the temple priesthood sit near the beneficiary end: they set the terms, administer the machinery, and collect the surplus, with arbitrage-grade exit (they can reinterpret what 'fulfilling Ma'at' requires). Peasant cultivators and corvee laborers sit at the target end: trapped exit, no mobility, bearing the tax/labor obligation with only contingent and often unmet promise of return. Provincial officials and the military occupy an intermediate structural position — they benefit from local power and crown provisioning but are themselves accountable for delivering the obligation downward, making them simultaneously enforcers and structurally exposed if the bargain visibly fails in their jurisdiction.
 *
 * MANDATROPHY ANALYSIS:
 *   The reciprocity reading is specifically structured to PREVENT classification as pure Snare: because failed obligations are declared (by this reading's own norm) to justify resistance or withdrawal of support, the constraint carries an internal correction mechanism absent from the divine mandate reading. This is why tangled_rope (genuine coordination — famine buffering, justice administration — coexisting with asymmetric extraction, requiring active enforcement) is the structurally correct claim rather than snare: the coordination function is real and independently attested (granary records, canal infrastructure), and the extraction ceiling is grounded in a norm that, when visibly breached, historically correlated with reduced compliance (First Intermediate Period nomarch inscriptions). A constraint whose extraction floats free of any corrective norm would push toward snare; this one does not, by the reading's own construction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_enforceability_ambiguity,
    'Was the reciprocity norm ever actually enforceable by the governed — i.e., did failed royal obligation historically produce organized withdrawal of support or resistance, or does the surviving discourse of ''broken obligation'' function only as post-hoc justification for revolts that had other causes (succession crisis, external invasion, elite factionalism)?',
    'Cross-reference famine/Nilometer records with contemporaneous administrative and provincial inscriptions during known periods of dynastic weakness (First and Second Intermediate Periods) to see whether withdrawal of compliance (tax non-payment, corvee evasion, nomarch autonomy claims) tracks documented failures of royal provision, or instead tracks elite power vacuums independent of provisioning failure.',
    'If withdrawal tracks provisioning failure, the reciprocity reading''s extraction ceiling is empirically real and this constraint is correctly bounded tangled_rope. If withdrawal tracks elite opportunism regardless of provisioning, the reciprocity norm functioned mainly as legitimating rhetoric for power struggles that would have occurred anyway, pushing the effective structure closer to a snare dressed in reciprocity language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_enforceability_ambiguity, empirical, 'Whether the reciprocity norm had real enforceable teeth or functioned as post-hoc legitimation for unrelated power struggles.').

omega_variable(
    kernel_framing_choice,
    'The Ma''at kernel could be authored as a single flat constraint (an ambiguous cosmic-order concept) or decomposed into the three contested readings used here (reciprocity, divine mandate, distributed maintenance). This story adopts the decomposed, reading-indexed framing per DP-001 ε-invariance.',
    'Compare ε and structural data across all three sibling story files: if they genuinely diverge (as authored — reciprocity moderate/bounded, divine mandate near-zero/immune, distributed maintenance diffuse/low-concentration), the decomposition is justified; if they converge, a single flat story would have sufficed.',
    'Confirms that treating ''Ma''at'' as three distinct constraints (rather than one constraint with an ambiguous observable) is the correct authoring choice, consistent with the BGS worked example''s guidance on decomposing conflated natural-language labels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Documents the committer-frame decomposition rationale for the Ma''at kernel across its three readings.').

omega_variable(
    who_certifies_breach,
    'Within the reciprocity reading, who has the socially recognized authority to declare that Pharaoh has breached the Ma''at obligation — is it the temple priesthood (who benefit from and administer the certification apparatus), provincial elites, or a more diffuse popular judgment expressed through non-compliance?',
    'Examine which actor''s testimony historically preceded or accompanied documented withdrawal-of-support episodes: priestly pronouncement, nomarch inscription, or generalized unrest without elite framing.',
    'If only the priesthood can certify breach, the reciprocity norm''s corrective mechanism is itself captured by a beneficiary group, weakening the tangled_rope claim toward something closer to elite-mediated extraction. If breach can be asserted more diffusely (including by non-elite non-compliance), the corrective mechanism is more genuinely distributed and the moderate extraction ceiling is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(who_certifies_breach, conceptual, 'Whether the authority to certify a breach of Ma''at is itself captured by an elite beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__reciprocity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__reciprocity_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__reciprocity_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__reciprocity_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__reciprocity_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__reciprocity_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__reciprocity_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__reciprocity_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__reciprocity_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__reciprocity_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__reciprocity_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__reciprocity_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__reciprocity_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__reciprocity_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__reciprocity_reading, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(maat_order_principle__reciprocity_reading, 0.12).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'Ma'at' per the ε-invariance principle. The divine_mandate_reading authors near-zero extraction ceiling and mountain-like immunity (Pharaoh cannot violate the standard by definition); the distributed_maintenance_reading diffuses the coordination function across all social stations rather than concentrating provisioning obligation on the throne, yielding a lower-concentration rope-like structure; this reciprocity_reading authors a moderate, norm-bounded extraction ceiling with an internal breach-and-resistance mechanism, yielding tangled_rope. The three share a kernel (maat_order_principle) but are NOT the same constraint — each has a distinct ε and distinct classification, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
