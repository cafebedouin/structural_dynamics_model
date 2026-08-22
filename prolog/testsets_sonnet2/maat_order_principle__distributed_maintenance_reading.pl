% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at as Distributed Maintenance Responsibility
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This story instantiates the distributed-maintenance reading of the Ma'at
 *   kernel: the claim that cosmic and social order is jointly sustained by
 *   proper conduct at every station, from Pharaoh through officials, priests,
 *   and commoners, rather than flowing exclusively downward from a
 *   divinely-embodying ruler or resting on a bilateral obligation contract
 *   between ruler and ruled. Evidence for this reading comes
 *   disproportionately from tomb autobiographies and wisdom literature
 *   produced by officials and scribes describing their own conduct as
 *   literally constitutive of Ma'at, distinct from royal inscriptions (which
 *   emphasize the divine-mandate reading) and from texts emphasizing royal
 *   duties of provision (the reciprocity reading). Because authority in this
 *   reading is grounded in demonstrated conduct rather than inherited status,
 *   extraction is comparatively low: even the Pharaoh's legitimacy is
 *   understood as earned through visible upholding of the standard, not
 *   simply asserted by cosmic fiat, and every station down to the commoner
 *   household has a genuine, non-trivial stake in the arrangement.
 *
 * KEY AGENTS:
 *   - pharaoh: apex maintainer, judged by the same conduct standard as lesser stations
 *   - viziers_and_high_officials: delegated administrative maintainers, personally accountable
 *   - temple_priesthoods: ritual maintainers, interpret proper conduct in cultic matters
 *   - local_officials_and_scribes: adjudicate and record fairness at district level
 *   - village_headmen: local mediators and organizers, legitimacy earned not appointed
 *   - commoner_households: baseline maintainers through honest personal conduct
 *   - foreign_peoples_and_captives: excluded from the community of maintainers entirely
 *   - modern_egyptologists: analytical observers reconstructing the reading from non-royal textual sources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.28).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.32).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at as Distributed Maintenance Responsibility").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '39131b2e-f388-4eb4-975d-7eb69586ce58').
narrative_ontology:cs_kernel_codification('39131b2e-f388-4eb4-975d-7eb69586ce58', distributed).
narrative_ontology:cs_authority_grounding('39131b2e-f388-4eb4-975d-7eb69586ce58', practice).
narrative_ontology:cs_interpretation_layer_present('39131b2e-f388-4eb4-975d-7eb69586ce58').
narrative_ontology:cs_reading_relation('39131b2e-f388-4eb4-975d-7eb69586ce58', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('39131b2e-f388-4eb4-975d-7eb69586ce58', maat_order_principle__reciprocity_reading, influences).
narrative_ontology:cs_axiom('39131b2e-f388-4eb4-975d-7eb69586ce58', foundational, legitimacy_earned_through_demonstrated_conduct).
narrative_ontology:cs_axiom_status(legitimacy_earned_through_demonstrated_conduct, holdable).
narrative_ontology:cs_axiom_grounding('39131b2e-f388-4eb4-975d-7eb69586ce58', legitimacy_earned_through_demonstrated_conduct, conventional).
narrative_ontology:cs_axiom('39131b2e-f388-4eb4-975d-7eb69586ce58', foundational, every_station_bears_genuine_maintenance_burden).
narrative_ontology:cs_axiom_status(every_station_bears_genuine_maintenance_burden, holdable).
narrative_ontology:cs_axiom_grounding('39131b2e-f388-4eb4-975d-7eb69586ce58', every_station_bears_genuine_maintenance_burden, conventional).
narrative_ontology:cs_reference_frame('39131b2e-f388-4eb4-975d-7eb69586ce58', conduct_grounded_station_legitimacy).
narrative_ontology:cs_drift_state('39131b2e-f388-4eb4-975d-7eb69586ce58', late_period_administrative_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('39131b2e-f388-4eb4-975d-7eb69586ce58', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, local_officials_and_scribes).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, temple_priesthoods).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, village_headmen).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, commoner_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, viziers_and_high_officials).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, village_headmen).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, commoner_households).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, cosmic_order_is_jointly_sustained).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, station_conduct_grounds_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits at the apex of the maintenance chain but, on this reading, is one maintaining node among many rather than the sole source of Ma'at. Judged by the same standard of proper conduct as any other station; loses legitimacy through misconduct exactly as a vizier or scribe would, just at greater scale and visibility. Cannot exit the standard without abandoning the throne's claim to rule.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh, beneficiary).

% Administer justice, taxation, and irrigation works as delegated maintainers of order. Their proper conduct in office is itself part of what sustains Ma'at; corruption or negligence is a direct violation attributable to them personally, not shielded by royal authority. Career and reputation are bound to visible upholding of the standard.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, viziers_and_high_officials, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, viziers_and_high_officials, payer).

% Perform daily rites understood as literal maintenance of cosmic order (the offering formula, temple upkeep) and interpret what proper conduct requires in ritual and civic matters. Gain authority and resources from being recognized maintainers, but that recognition depends on visibly discharging the function rather than resting on inherited status alone.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, temple_priesthoods, agenda_setter,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, temple_priesthoods, beneficiary).

% Record contracts, adjudicate local disputes, and enforce customary fairness at the village and district level. Their standing rests on demonstrated fair dealing in their station; a corrupt scribe is understood to personally disorder Ma'at, not merely to break a rule handed down from above.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, local_officials_and_scribes, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, local_officials_and_scribes, beneficiary).

% Mediate disputes and organize communal labor and harvest-sharing at village level, treated as legitimate local maintainers of order in their own right, independent of direct royal appointment. Bear the cost of failure in reduced standing and communal sanction if their conduct is seen as improper.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, village_headmen, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, village_headmen, payer).

% Honest dealing, respect for boundaries, care for family and neighbors, and fair labor are understood as the commoner's own contribution to sustaining cosmic order, not merely obedience to superiors. Gain a genuine stake in the system's legitimacy — their proper conduct counts — but bear real costs when others (including the Pharaoh) fail their station without commoners having power to sanction the top of the chain directly.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, commoner_households, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, commoner_households, payer).

% Categorized outside the community of Ma'at-maintainers entirely (as bearers of isfet, disorder) in much surviving textual material. Their conduct is not counted as a station within the distributed-maintenance frame, and they have no voice in defining what proper conduct requires — the frame's inclusiveness stops at its own boundary.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, foreign_peoples_and_captives, excluded,
    powerless, immediate, trapped, regional).

% Reconstruct the distributed-maintenance reading from tomb autobiographies, wisdom literature (e.g. the Instruction of Ptahhotep, Instruction to Merikare), and administrative texts in which officials and commoners alike claim credit for upholding Ma'at in their station, distinct from royal inscriptions emphasizing the king's unique cosmic role.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, modern_egyptologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes the burden of sustaining social and cosmic order across every station in society — administrative, ritual, and domestic — so that order does not depend solely on the continuous correct action of a single ruler, and gives every actor, including the powerless, a legible standard by which their own conduct matters.
% TRANSFER_FUNCTION: Moves the burden (and the credit) of maintaining order outward from a single royal point across the whole administrative and social hierarchy; officials, priests, and commoners each absorb a share of the labor of upholding fairness, ritual correctness, and honest dealing, and each can claim some of the resulting legitimacy in return.
% ABSENT_VOICES: Foreign peoples and captives are structurally excluded from the community of maintainers — their conduct is not evaluated as a station-contribution to Ma'at and they have no standing to object to the boundary that excludes them, since surviving sources are produced entirely by and for those inside the community of maintainers.
% DISAPPEARANCE_RATIONALE: If the distributed-maintenance frame vanished, officials and commoners could still be evaluated instrumentally (competent/incompetent, compliant/non-compliant), but they would lose the specifically Ma'at-grounded claim that their own conduct in a low station literally sustains cosmic order — some administrative behavior might barely change, but the legitimating language available to a scribe or village headman to explain why fairness matters at their level would need to be replaced by something else (divine command, personal virtue ethics, custom). Whether the world 'rearranges' or stays functionally similar under a different vocabulary is exactly the kind of question the sibling readings would answer differently.
% FOUNDING_PROBLEM: A single ruler and a small royal court cannot personally enact fairness, ritual correctness, and social stability across a large, dispersed population and administration — order has to be produced by many hands, most of them far from the throne, most of the time.
% FOUNDING_PROBLEM_CORROBORATION: Tomb autobiographies of officials (e.g. self-descriptions as one who 'did Ma'at' in office, gave bread to the hungry, judged fairly) attest the problem and the distributed solution from inside the official class. Wisdom literature aimed at training scribes and officials (Ptahhotep, Merikare) corroborates it as a live pedagogical concern rather than mere royal propaganda. No source from outside the literate administrative and priestly classes survives to corroborate independently — commoner and excluded voices are absent from the corroborating record, which is itself a limit on how far the 'distributed' claim can be verified rather than merely asserted by its beneficiaries.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, contested).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.28) because the reading's defining structural claim is that legitimacy is earned through demonstrated conduct at every level rather than extracted by virtue of station alone — this is the lowest-extraction reading of the three kernel readings by design, per the expected structural delta. Suppression is moderate (0.32): the standard is genuinely internalized and self-reinforcing (wisdom literature functions as socialization, not coercion), but it is not absent — failure to conform to station-conduct norms carries real social and administrative penalty. Theater ratio is moderate and rising slightly (0.30 to 0.40) reflecting that as the administrative apparatus solidifies over the interval, autobiographical and commemorative conduct-claims (tomb inscriptions asserting 'I did Ma'at') increasingly function as status performance for officials seeking commemoration, alongside their genuine coordinating function. Accessibility collapse is moderate (0.45): alternative frames for grounding one's conduct (kinship obligation, personal reputation, local custom) coexist with the Ma'at frame rather than being fully displaced by it. Resistance is moderate-low (0.35): the frame is widely embraced by those it includes because it grants them genuine standing, so resistance comes mainly from the boundary the frame draws (see absent_voices) rather than from those inside it.
 *
 * DIRECTIONALITY LOGIC:
 *   Every stakeholder inside the community of maintainers carries a dual role (agenda_setter/beneficiary or beneficiary/payer) because the reading's core structural claim is that conduct and legitimacy flow in both directions at every station — even the Pharaoh both sets the standard at the top and is subject to it, which is precisely what distinguishes this reading from the divine_mandate reading. Directionality sits closer to symmetric across most seats: officials and commoners get real standing (low-to-moderate d) in exchange for real behavioral constraint (moderate d), which is why extraction stays low system-wide. The excluded seat (foreign_peoples_and_captives) is the exception: fully outside the accounting, trapped, with no coordination benefit and no voice — the frame's low internal extraction is purchased partly by drawing its boundary to exclude them from the ledger entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a single ruler cannot personally enact fairness across a whole population) remains structurally live in the sense that large-scale order still requires distributed administrative labor — the mismatch check (status=contested, verdict=contested) reflects that this is not a resolved mandatrophy case: the frame's function was never fully supplanted by pure force or pure ritual performance, but the corroboration record is limited entirely to the literate administrative and priestly classes who benefit from the frame, which is itself evidence the classification should sit at moderate confidence rather than be read as a settled 'genuine coordination, case closed' verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_reading_source_bias,
    'Is the distributed-maintenance reading a genuine alternate strand of Ma''at ideology attested independently of royal control, or is it primarily a self-serving frame produced by the literate official class to claim reflected cosmic legitimacy for their own administrative conduct?',
    'Comparative analysis of the social range of sources: tomb autobiographies and wisdom literature skew heavily toward literate officials and priests. Broader corroboration would require evidence from non-official, non-priestly, or non-literate contexts (village-level legal papyri, worker settlements like Deir el-Medina) that use the same conduct-based legitimating vocabulary independent of official commemorative genres.',
    'If the reading is substantially self-serving official-class ideology, its low authored extraction may understate how much the frame functioned to launder official misconduct as personal virtue-performance in tomb inscriptions (raising the true theater_ratio); if genuinely broad-based, the low-extraction, distributed-accountability reading holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_reading_source_bias, empirical, 'Whether the distributed-maintenance reading is genuinely broad-based or an official-class self-legitimation strand within Ma''at ideology.').

omega_variable(
    maat_kernel_reading_coexistence,
    'Do the distributed_maintenance, divine_mandate, and reciprocity readings of Ma''at represent genuinely distinct ideological strands held by different actors or textual traditions simultaneously, or do they represent the SAME actors deploying different readings situationally (e.g., a Pharaoh invoking divine_mandate in monumental inscriptions while officials invoke distributed_maintenance in tomb autobiographies, as complementary rather than competing claims)?',
    'Genre-by-genre textual analysis: track which reading appears in which text-type (royal monumental vs. official funerary vs. legal/administrative) and whether any single actor invokes more than one reading across different contexts.',
    'If situational rather than factional, the three kernel readings may function less as competing claims about the world and more as register-specific vocabularies for different audiences — this would not change any single reading''s authored ε, but would affect how much weight the corpus should place on inter-reading network edges (influences vs. coexists_with) versus treating them as fully independent ideological positions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maat_kernel_reading_coexistence, conceptual, 'Whether the three Ma''at kernel readings are held by distinct factions or deployed situationally by the same actors across genres.').

omega_variable(
    excluded_boundary_naturalization,
    'Is the exclusion of foreign peoples and captives from the community of Ma''at-maintainers a stable, unexamined boundary condition of the distributed-maintenance reading throughout the period, or did the boundary itself shift (e.g., under foreign-origin dynasties, or through incorporation via cultic/administrative integration)?',
    'Track treatment of foreign-born officials and integrated populations across periods (e.g., the Hyksos period, Nubian officials under the New Kingdom) for evidence that the maintainer-boundary was permeable under specific conditions rather than fixed by ethnicity.',
    'If the boundary was permeable and conduct-based even for foreign-origin actors, the distributed_maintenance reading''s inclusiveness claim would need to be read more literally (grounded in conduct, not origin), which would support a lower accessibility_collapse than currently authored. If the boundary was rigid regardless of conduct, the reading''s egalitarian self-description is itself partly theatrical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_boundary_naturalization, empirical, 'Whether the excluded-foreigner boundary was fixed by origin or permeable via demonstrated conduct, which bears on how literally to take the reading''s conduct-based inclusiveness claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__distributed_maintenance_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__distributed_maintenance_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__distributed_maintenance_reading, theater_ratio, 80, 0.39).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__distributed_maintenance_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 60, 0.27).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 80, 0.275).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(maat_order_principle__distributed_maintenance_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__distributed_maintenance_reading, 0.1).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the single natural-language label 'Ma'at' into structurally distinct kernel readings sharing one contested kernel (maat_order_principle): distributed_maintenance_reading (this story, lowest ε — authority earned through demonstrated conduct at every station), divine_mandate_reading (authority inherent in the Pharaoh as cosmic embodiment, highest ε expected — the ruler cannot be held accountable by definition), and reciprocity_reading (moderate ε — a bilateral obligation contract in which the Pharaoh must provide justice and resources or forfeit legitimacy). Per the ε-invariance principle, these are not one constraint measured three ways but three constraints sharing a textual label, linked here via network edges rather than merged into a single averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
