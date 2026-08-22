% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Divine Mandate Reading of Ma'at: Pharaoh as Embodied Source of Cosmic Order
 *   domain: religious/political
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested Ma'at kernel: the
 *   divine-mandate reading, in which cosmic order (Ma'at) flows from the
 *   divine realm through the pharaoh into society, and the ruler embodies
 *   rather than merely administers that order. Under this reading the pharaoh
 *   sits structurally outside the constraint — Ma'at cannot be violated by
 *   the ruler by definition, because the ruler IS its earthly instantiation.
 *   This forecloses any seat, priestly or popular, from which royal command
 *   could be assessed against Ma'at as an external standard. The reading is
 *   authored here as a single, ε-stable constraint; it does not average over
 *   or hedge against the sibling readings (reciprocity,
 *   distributed-maintenance), which are separate constraint stories with
 *   their own ε values and stakeholder structures per the ε-invariance
 *   principle.
 *
 * KEY AGENTS:
 *   - pharaonic_office: source of the order rather than subject to it — collects labor, tribute, and legitimacy without a standard that could indict royal conduct
 *   - temple_priesthood: co-produces and stages the theology, receiving endowments in exchange for confirming the mandate reading's dominance
 *   - corvee_laborers and provincial_peasantry: bear the extraction the reading recasts as cosmic maintenance
 *   - conquered_populations: absorbed under the reading's frontier logic of order-against-chaos
 *   - modern_historians: analytical seat reconstructing the reading's function from outside it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.71).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.86).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Divine Mandate Reading of Ma'at: Pharaoh as Embodied Source of Cosmic Order").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "religious/political").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '50accacd-c3ea-4bd8-8f40-3b83e6bfaffe').
narrative_ontology:cs_kernel_codification('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', distributed).
narrative_ontology:cs_authority_grounding('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', extraction).
narrative_ontology:cs_interpretation_layer_present('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe').
narrative_ontology:cs_reading_relation('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', foundational, ruler_is_maat_not_bound_by_it).
narrative_ontology:cs_axiom_status(ruler_is_maat_not_bound_by_it, holdable).
narrative_ontology:cs_axiom_grounding('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', ruler_is_maat_not_bound_by_it, theological).
narrative_ontology:cs_axiom('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', secondary, royal_action_cannot_constitute_isfet).
narrative_ontology:cs_axiom_status(royal_action_cannot_constitute_isfet, holdable).
narrative_ontology:cs_axiom_grounding('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', royal_action_cannot_constitute_isfet, theological).
narrative_ontology:cs_reference_frame('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', old_kingdom_centralized_kingship).
narrative_ontology:cs_drift_state('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', first_intermediate_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('50accacd-c3ea-4bd8-8f40-3b83e6bfaffe', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaonic_office).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, royal_court).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, temple_priesthood).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, corvee_laborers).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, provincial_peasantry).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, conquered_populations).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, cosmic_order_requires_single_embodied_source).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, royal_infallibility_by_definition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declared the living embodiment of Ma'at itself, standing outside the order it maintains rather than subject to it. Issues decrees, commands labor levies, and directs temple and military resources, all framed as the necessary maintenance of cosmic balance. Because the ruler IS Ma'at by definition in this reading, no royal act can be assessed against Ma'at as an external standard — there is no seat from which to charge the pharaoh with violating it.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaonic_office, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, pharaonic_office, beneficiary).

% Administers the ritual apparatus that stages and confirms the pharaoh's cosmic role — temple endowments, festival cycles, oracular pronouncements — and receives land grants, labor, and tribute in exchange for producing the theology that makes the divine-mandate reading self-sealing. Their material position depends on this reading remaining dominant over the reciprocity and distributed-maintenance readings.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, temple_priesthood, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, temple_priesthood, agenda_setter).

% Officials, viziers, and nobles who administer royal decrees and collect the surplus that flows upward under the mandate's authority. They benefit from the reading's suppression of any standard by which royal command could be second-guessed, since their own directives ride on the same unquestionable authority.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, royal_court, beneficiary,
    organized, generational, constrained, national).

% Conscripted for monument construction, canal work, and military logistics under royal command justified as cosmic necessity — building tombs and temples that manifest Ma'at through the ruler. Because the mandate reading places the pharaoh's will beyond challenge, there is no available claim that a specific levy is excessive or unjust; to resist is to resist cosmic order itself.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, corvee_laborers, payer,
    powerless, biographical, trapped, local).

% Renders grain and produce as tribute administered through nomarchs and temple estates, sanctified as the flow of order from the divine source through the ruler to the land. Poor harvests or excessive extraction cannot be framed as royal failure under this reading, since the ruler embodies rather than administers Ma'at — any shortfall is displaced onto other causes (impurity, foreign intrusion, priestly neglect) rather than royal policy.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, provincial_peasantry, payer,
    powerless, generational, trapped, regional).

% Absorbed into the tribute and labor system following military campaigns framed as extending Ma'at against chaos (isfet) at the frontiers. Their subjugation is narrated as cosmological necessity rather than conquest, foreclosing any register in which their loss could be named as extraction rather than restoration of order.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, conquered_populations, payer,
    powerless, biographical, trapped, continental).

% Alternative theological-political framings — that Ma'at imposes mutual obligation on the ruler, or that its maintenance is distributed across all social stations — persist in wisdom literature, tomb autobiographies, and local cult practice, but are structurally subordinated whenever the divine-mandate reading dominates court ideology and royal inscription. They surface in moments of royal weakness or dynastic crisis but have no standing seat in the mandate reading's own framework.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, reciprocity_and_distributed_readings, excluded,
    moderate, generational, constrained, national).

% Reconstruct the theology from royal inscriptions, temple records, and administrative papyri, comparing periods of centralized royal ideology (Old/Middle Kingdom) against periods where reciprocal or distributed framings visibly gain ground (First Intermediate Period literature, Instruction texts). They can observe the mandate reading's suppression function without occupying any seat inside it.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, modern_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaonic_office).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unambiguous source of cosmological and political authority, eliminating the coordination problem of competing claims to legitimate rule by defining the ruler as the order itself rather than one interpreter among several.
% TRANSFER_FUNCTION: Moves labor, grain, and tribute from laboring and provincial populations upward through royal and temple administration, justified as sustaining Ma'at rather than as a political transfer subject to negotiation or limit.
% ABSENT_VOICES: The reciprocity reading (Ma'at obligates the ruler to provide justice and stability in exchange) and the distributed-maintenance reading (Ma'at is sustained by conduct at every social station) both exist in the same textual corpus but are structurally excluded from adjudicating royal conduct whenever the mandate reading is dominant — they surface mainly in wisdom literature and periods of dynastic weakness, never as a standing check.
% DISAPPEARANCE_RATIONALE: If the divine-mandate reading collapsed, royal command would lose its self-sealing justification and would need to be defended on reciprocal or distributed grounds — i.e., against a standard the ruler could actually fail to meet. Labor levies, tribute rates, and military campaigns would become contestable in a way the mandate reading forecloses; this is precisely what appears to happen rhetorically during intermediate periods when royal authority weakens.
% FOUNDING_PROBLEM: Early Egyptian political consolidation needed to unify fragmented regional cults and administrative centers under one authority without a competing external legal or religious standard that could be invoked against the ruler by rival power centers (priesthoods, nomarchs, foreign claimants).
% FOUNDING_PROBLEM_CORROBORATION: Egyptological reconstruction from administrative and literary sources outside the royal-temple complex (e.g., First Intermediate Period laments, Instruction of Amenemope-adjacent wisdom texts, and comparative work on divine kingship in neighboring Bronze Age polities) treats the unification problem as long resolved by the era the mandate reading is most heavily inscribed, suggesting the reading's persistence tracks the interests of the court and priesthood rather than an ongoing coordination need. No source from within the beneficiary set (royal inscriptions, temple theology) attests the problem as dead — they uniformly narrate it as perpetually live and cosmically urgent, which is itself the pattern a self-sealing legitimacy claim would produce.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high-moderate (0.71) because real resources (labor, grain, land, conquered populations' output) flow upward and the mandate reading provides no internal mechanism by which that flow could be capped or contested — it only provides justification. Suppression is authored higher still (0.86) because the reading's central move is not merely extracting resources but foreclosing the very possibility of a standard against which the ruler's extraction could be judged; this is a suppression of an entire evaluative frame, not just of specific resistance acts. Theater ratio rises across the interval (0.35 to 0.58) reflecting the increasing weight of ritual and monumental confirmation relative to any residual coordination function, consistent with the pattern where centralized theology intensifies as administrative capacity to actually deliver stability wanes in later periods. Accessibility collapse is authored very high (0.80) because once a subject accepts the ruler-as-embodiment premise, no alternative political vocabulary remains available from within that frame — this is the reading's central mechanism, not incidental to it.
 *
 * PERSPECTIVAL GAP:
 *   From the pharaonic and priestly seats, the arrangement is not extraction at all but the necessary, self-evident maintenance of cosmic order — there is no vantage inside the reading from which royal command could register as a cost. From the corvee laborer or provincial peasant seat, the same flows of labor and grain operate as involuntary transfer with no available recourse, precisely because the reading that would let them contest it is the one being denied a seat. The engine should compute this divergence structurally from the beneficiary/victim/exit declarations, not from any claim asserted here.
 *
 * DIRECTIONALITY LOGIC:
 *   The pharaonic office and its administering apparatus (royal court, temple priesthood) are declared structural beneficiaries: labor and tribute flow to them, and their authority is unconditioned by any Ma'at-derived standard they could fail. Laboring and provincial populations, and conquered populations at greater spatial scope, are declared victims: they bear the transfer and have no available claim against it within this reading's own terms — their exit options are trapped, not merely constrained, because leaving the tribute/labor system means leaving the cosmological order itself as this reading defines it. This differs sharply from the reciprocity reading (where the ruler owes something back, giving the payer seats at least a claim) and the distributed-maintenance reading (where the payer seats are themselves co-producers of order, not merely subjects of it) — the divine-mandate reading structurally forecloses both of those claims, which is precisely the expected delta for this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored dead: the coordination problem this ideology was built to solve (unifying fragmented regional authority under one legitimacy claim) was largely resolved by the era in which the divine-mandate theology is most heavily inscribed, yet the reading persists and intensifies — rising theater_ratio and suppression_requirement across the interval track a mandate whose stated function has receded while its extractive operation has not. This is not proof the reading was always pure extraction (the founding_problem_status is dead, not contested, only because corroboration from outside the beneficiary set treats the original unification need as resolved); it is exactly the kind of divergence the mandatrophy analysis exists to flag rather than adjudicate on its own.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_reading_as_dominant_or_singular,
    'Was the divine-mandate reading ever the SINGLE operative theology, or did it always coexist in tension with reciprocity and distributed-maintenance framings even within elite discourse (e.g., Instruction texts, tomb autobiographies claiming personal Ma''at-conduct)?',
    'Comparative textual analysis across genres (royal inscription vs. wisdom literature vs. private tomb biography) and across periods (centralized vs. intermediate) to establish whether the mandate reading''s dominance is period-specific or constant.',
    'If the mandate reading was never truly singular, its authored suppression value may be overstated for the periods where reciprocity/distributed framings had genuine practical purchase (e.g., local justice administration); if it was genuinely dominant during peak centralization, the suppression figure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_reading_as_dominant_or_singular, empirical, 'Whether the divine-mandate reading was ever the sole operative theology or always contested.').

omega_variable(
    natural_cosmology_vs_constructed_ideology,
    'Is the pharaoh-as-embodiment-of-Ma''at claim best read as sincere ancient cosmology (a genuine metaphysical belief structuring the whole society, not a tool anyone consciously wields) or as constructed legitimating ideology serving identifiable court and priestly interests?',
    'Cross-reference royal ideology''s flexibility under dynastic stress (does the claim bend or break when royal authority weakens, as in Intermediate Periods?) against genuinely fixed natural-law-like claims that show no such flexibility.',
    'If the claim flexes systematically with royal fortunes, this supports the constructed-ideology reading (consistent with the tangled_rope classification authored here); if it holds constant regardless of material conditions, a more mountain-like naturalized-order reading would be defensible, which is precisely the false-summit boundary this story''s beneficiary declarations are meant to test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_cosmology_vs_constructed_ideology, conceptual, 'Whether the mandate reading is sincere cosmology or interest-serving construction — bears directly on FSM-adjacent boundary even though this story is authored as tangled_rope, not mountain.').

omega_variable(
    framing_choice_pharaoh_source_vs_subject,
    'The kernel could be framed with the pharaoh as either the SOURCE of Ma''at (this reading) or as its most privileged SUBJECT bound by exceptional standards (a variant closer to reciprocity). Which framing better matches the primary evidence (royal titulary, coronation ritual, temple relief programs)?',
    'Systematic review of coronation and jubilee (heb-sed) ritual texts to determine whether the ritual language positions the pharaoh as manifesting Ma''at ex nihilo or as being ritually purified/tested against a standard that could, in principle, register failure.',
    'If ritual language consistently shows testing/renewal logic (rather than pure manifestation), the reciprocity reading may be the better-evidenced default and this divine-mandate reading would be the more ideologically loaded of the two, affecting relative confidence rather than the ε values themselves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_pharaoh_source_vs_subject, conceptual, 'Alternative framing of the kernel (source vs. tested subject) and what evidence would favor each — routed here per the CS-framing under-determination guidance rather than left implicit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__divine_mandate_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__divine_mandate_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__divine_mandate_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__divine_mandate_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__divine_mandate_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__divine_mandate_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__divine_mandate_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__divine_mandate_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__divine_mandate_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__divine_mandate_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__divine_mandate_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__divine_mandate_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__divine_mandate_reading, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__divine_mandate_reading, suppression_requirement, 80, 0.84).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__divine_mandate_reading, suppression_requirement, 100, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__divine_mandate_reading, 0.08).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the maat_order_principle kernel. divine_mandate_reading (this file) authors high extraction/high suppression consistent with the ruler-as-source-of-order framing. reciprocity_reading authors a structurally different constraint in which the ruler owes deliverables back to the population, producing a different beneficiary/victim balance and likely lower suppression (a real, if asymmetric, coordination function). distributed_maintenance_reading authors yet another constraint in which the payer seats are themselves co-producers of order rather than mere subjects, which should produce the lowest extraction of the three. All three share the same kernel_id (maat_order_principle) but are NOT the same constraint — each has its own ε, stakeholders, and classification, linked here rather than merged, per the ε-invariance and kernel-reading rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
