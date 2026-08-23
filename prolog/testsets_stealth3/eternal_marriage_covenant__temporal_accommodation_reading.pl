% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Temporal Accommodation Reading of the Eternal Marriage Covenant: Practice Suspended, Doctrine Retained
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   In 1890 the First Presidency announces that plural marriage will cease as
 *   open practice, in response to a federal campaign of statutes, seizures,
 *   and imprisonments that threatens the church's corporate existence. The
 *   announcement suspends the practice without renouncing the doctrine: the
 *   eternal-marriage revelation remains canon, and obedience to the law of
 *   the land is taught as taking precedence for the duration of the political
 *   constraint, with restoration anticipated when constraint lifts. The
 *   interval runs t=0 (1890) to t=40 (1930), tracking the accommodation's
 *   issuance, its ambiguous early enforcement, the 1904 hardening (Second
 *   Manifesto, Smoot hearings, apostolic resignations), and consolidation
 *   into a permanent prohibition. Epsilon's referent is the standing
 *   arrangement under contest: the suspension-with-retained-doctrine regime
 *   itself, assessed by this reading's own lights. The claimed type and the
 *   metrics are independently authored facts: I claim tangled_rope because
 *   the structure holds a genuine survival-coordination function AND
 *   asymmetric extraction from believers AND active dual enforcement; the
 *   metrics describe the operation I judge descriptively true. The engine
 *   computes per-seat types from the structural data; divergence between my
 *   claim and computed seats is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - first_presidency_leadership: Agenda-setter (institutional/constrained) — issues and administers the suspension under duress; collects institutional survival alongside the cost of managing the doctrinal double bind.
 *   - federal_government: Coercive counterparty and agenda-setter (institutional/arbitrage) — defines the enforcement boundary the accommodation defers to; collects the compliance outcome and holds unconstrained escalation options.
 *   - lds_church_institution: Primary collected seat (institutional/constrained) — corporate survival, property retention, and the statehood path are what the arrangement purchases.
 *   - devout_plural_marriage_believers: Primary bearing seat (powerless/identity_locked) — forgo commanded practice they hold eternally binding; exit costs are spiritual and social, not logistical.
 *   - existing_plural_families: Bearing seat with residual offset (powerless/trapped) — bear legal non-recognition and stigma for pre-existing covenants while gaining declining prosecution risk.
 *   - performing_priesthood_holders: Elite bearing seats (powerful/identity_locked) — officiators deposed or resigned when the public-private gap was audited.
 *   - mexico_colony_settlers: Partial-exit bearing seats (moderate/mobile) — relocate beyond statute reach; their mobility is jurisdictional, costly, and ultimately reversed.
 *   - rank_and_file_membership: Dual-positioned beneficiary-payers (organized/identity_locked) — trade practice for statehood, safety, and communal continuity.
 *   - restorationist_dissenters: Excluded voice (powerless/identity_locked) — deny any earthly authority to pause an eternal command; outside the conversation that set the suspension's terms.
 *   - senate_smoot_committee: Analytical observer (institutional/analytical) — audits the public-private gap and forces the hardening that converts announcement into enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.58).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.6).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Temporal Accommodation Reading of the Eternal Marriage Covenant: Practice Suspended, Doctrine Retained").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, 'b59080fb-9bfc-44a2-b62d-96efc7812562').
narrative_ontology:cs_kernel_codification('b59080fb-9bfc-44a2-b62d-96efc7812562', fixed_text).
narrative_ontology:cs_authority_grounding('b59080fb-9bfc-44a2-b62d-96efc7812562', lineage).
narrative_ontology:cs_interpretation_layer_present('b59080fb-9bfc-44a2-b62d-96efc7812562').
narrative_ontology:cs_reading_relation('b59080fb-9bfc-44a2-b62d-96efc7812562', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('b59080fb-9bfc-44a2-b62d-96efc7812562', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_axiom('b59080fb-9bfc-44a2-b62d-96efc7812562', foundational, eternal_command_valid_while_practice_deferred).
narrative_ontology:cs_axiom_status(eternal_command_valid_while_practice_deferred, holdable).
narrative_ontology:cs_axiom_grounding('b59080fb-9bfc-44a2-b62d-96efc7812562', eternal_command_valid_while_practice_deferred, theological).
narrative_ontology:cs_axiom('b59080fb-9bfc-44a2-b62d-96efc7812562', foundational, civil_law_precedence_during_coercion_intervals).
narrative_ontology:cs_axiom_status(civil_law_precedence_during_coercion_intervals, holdable).
narrative_ontology:cs_axiom_grounding('b59080fb-9bfc-44a2-b62d-96efc7812562', civil_law_precedence_during_coercion_intervals, conventional).
narrative_ontology:cs_reference_frame('b59080fb-9bfc-44a2-b62d-96efc7812562', eternal_command_deferred_not_revoked).
narrative_ontology:cs_drift_state('b59080fb-9bfc-44a2-b62d-96efc7812562', post_second_manifesto_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b59080fb-9bfc-44a2-b62d-96efc7812562', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, lds_church_institution).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, first_presidency_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_membership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, federal_government).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, devout_plural_marriage_believers).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, performing_priesthood_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, mexico_colony_settlers).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, twelfth_article_of_faith_civil_obedience).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, dc134_government_as_divine_instrument).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, constitutional_supremacy_in_religious_matters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the keys over sealing ordinances and announces in 1890 that authorizations of new plural marriages will cease. Drafts the announcement in deliberately narrow wording so it addresses practice, not doctrine. Continues teaching the eternal-marriage revelation as scripture while directing members to obey anti-bigamy statutes. Bears accusation from both directions: outsiders read the announcement as evasion, stricter believers read it as surrender. Leaving would mean abandoning the institution they head or forcing a doctrinal rupture; neither is available without destroying what they steward.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, first_presidency_leadership, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, first_presidency_leadership, beneficiary).

% Congress and the federal courts built the pressure that produced the announcement: anti-bigamy statutes, the Edmunds-Tucker Act's disincorporation and forfeiture provisions, marshals, and prison terms. Collects the delivered outcome: plural marriage ceases as open practice, federal law prevails, and Utah enters the union on that footing. Holds escalatory options throughout and can reopen enforcement at will; nothing in the arrangement constrains it.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, beneficiary).

% The corporate body facing disincorporation and asset forfeiture in 1890. Under the arrangement it survives with properties intact, regains a path to statehood for its territorial base, and keeps its membership rolls and tithing base. Its survival is the arrangement's principal collected result.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, lds_church_institution, beneficiary,
    institutional, generational, constrained, regional).

% Ordinary members gain an end to raids, arrests, and social war with the wider nation, plus Utah statehood in 1896. They pay in the strain of professing an eternal principle whose practice is publicly halted, in temple-interview questions about compliance, and in expected loyalty to leaders who announced the change without member deliberation. Leaving the faith would cost them family, community, and, by their own lights, exaltation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_membership, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_membership, payer).

% Members who receive the eternal-marriage revelation as binding divine law and either practice it or aspire to it. After the announcement they may contract no new plural marriages, though the revelation remains canon. Their options are silence, relocation to colonies outside United States jurisdiction, or private continuation that risks discipline. Their stake is framed in eternal terms: foregone practice touches what they believe determines exaltation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, devout_plural_marriage_believers, payer,
    powerless, generational, identity_locked, regional).

% Households formed before 1890. Their covenants remain doctrinally in force but legally unrecognized; they live with stigma, economic fragility, and periodic legal exposure while the community's public face declares the practice ended. Prosecution risk recedes over time and the settlement shields them from renewed raids, a partial offset to costs they did not choose and cannot escape, since dissolving the covenants is not available to them.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families, beneficiary).

% Apostles and other officiators who perform sealings after the announcement, holding that the eternal command outweighs the political truce. When the gap between public profession and private practice surfaces in the Smoot hearings, they face resignation, deposition, or recantation; several lose offices and standing in the quorum. Their sense of self is bound to the priesthood office they would forfeit.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, performing_priesthood_holders, payer,
    powerful, biographical, identity_locked, regional).

% Families who move to Mormon colonies in northern Mexico and Canada, where United States anti-bigamy statutes do not reach, sustaining new plural households through the 1890s and 1900s. Their mobility is real but partial: they remain under church authority, endure frontier hardship, and are ultimately displaced when revolution in 1912 and extending church discipline reach the colonies.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, mexico_colony_settlers, payer,
    moderate, biographical, mobile, continental).

% The Senate panel investigating from 1904 onward whether Apostle Reed Smoot represents a church still practicing plural marriage. It subpoenaes leadership testimony, exposes post-announcement sealings and denials, and extracts the assurances that harden the suspension. Watches the public-private gap from outside the faith; its published record is the era's richest audit trail.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, senate_smoot_committee, observer,
    institutional, biographical, analytical, national).

% Believers, inside and later outside the church, who hold that no earthly authority can pause an eternal command and that the announcement exceeded prophetic authority. Their objection is not admitted into official channels; they persist through private networks, ordination-claim lineages, and eventually organized communities after 1929.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, restorationist_dissenters, excluded,
    powerless, generational, identity_locked, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, lds_church_institution).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the believing community's collective-action crisis under existential federal threat (disincorporation, asset forfeiture, mass incarceration): a single unified compliance posture that preserves the corporate church, ends prosecutions, and holds the covenant community together while keeping the doctrinal claim officially intact.
% TRANSFER_FUNCTION: Moves practice-rights and public doctrinal consistency-in-practice from devout believers and performing priesthood holders to the federal state (delivered compliance) and to the church institution (corporate survival, property retention, Utah statehood). Transfers personal arrest risk from leadership to private conscience, and moves doctrinal certainty into managed ambiguity.
% ABSENT_VOICES: Restorationist dissenters who deny any earthly authority can pause an eternal command were outside the room where the announcement's terms were drafted and proclaimed; women in existing plural marriages held no formal seat, and the suspension was announced to them rather than negotiated with them. Their objections surface later in private correspondence, colony settlements, and eventually the fundamentalist schism.
% DISAPPEARANCE_RATIONALE: If the suspension arrangement vanished overnight (authorization of practice resumed openly), federal escalation would restart immediately: renewed prosecutions under dormant statutes, revived disincorporation proceedings, and loss of the statehood-era political settlement. The church's monogamous public order and its legal-personhood arrangements would reorganize around open conflict with the state.
% FOUNDING_PROBLEM: The institutional church's existential collision with federal anti-polygamy enforcement, the Edmunds-Tucker regime of disincorporation, property forfeiture, and imprisonment, threatened the survival of the covenant community as an organized body.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties, the Senate's Smoot hearing record corroborates that the enforcement emergency driving the 1890 announcement had subsided by 1907 (the committee seated Smoot after extracting written assurances), and the standard historical treatments of the post-Manifesto period attest that the founding emergency ended while the suspension persisted. The First Presidency attests the opposite framing (doctrine dormant, restoration possible), but no source outside the benefiting parties attests that the founding problem remained live.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) is substantial but bounded: what is taken is practice-rights and consistency-in-practice from people whose theology prices them eternally, plus the residual legal limbo of existing families; the series peaks at the abrupt 1890 deprivation (0.70), bumps at the 1904 loophole-closing (0.68), then eases as a generation socialized into monogamy replaces the deprived cohort and the most costly believers exit into schism. Suppression (0.60 end-state, series 0.80 down to 0.60) traces enforcement migration: peak federal coercion at issuance, a dip through the statehood bargain, a rise at 1904 when the church adopts internal enforcement while federal threat lingers, then decay to stable internal discipline. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled downstream. Theater_ratio runs INVERTED against the usual drift pattern: high early (0.55) because official statements diverged sharply from privately permitted and performed sealings, falling steeply after the 1904-07 audit cycle converts announcement into sincere enforcement (0.25 by 1930). Accessibility_collapse (0.62): alternatives once understood largely closed (secret practice punished, defiance met with deposition, renunciation identity-unthinkable), though jurisdictional flight to colonies remained partially viable until events destroyed it. Resistance (0.50): covert post-announcement marriages, dissenting apostles, and the emerging restorationist line constitute real but dispersed resistance; coalition potential among the bearing seats was structurally blocked by dispersion, identity-lock, and leadership mediation of all official channels. The three series share one time grid (0,5,10,14,20,26,33,40) so every metric is authored at every examined point. Trajectories are phased-monotone, not oscillating: the driver is enforcement migration, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seats the same structure reads as faithful stewardship under compulsion: costly, reluctant, doctrine-preserving, the least-bad available branch. From the identity-locked bearing seats it reads as commanded practice withheld under duress with no legitimate channel of appeal. The excluded restorationist seat reads betrayal of revelation; the federal seat reads closure of an open wound in constitutional order. The engine computes these per-seat classifications from power, exit, and declaration data; nothing here adjudicates which phenomenology is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. The presidency, the corporate institution, and the federal government sit near the beneficiary pole (declared beneficiaries; the presidency additionally administers, the federation holds arbitrage-grade escalation). Rank-and-file members carry a dual declaration (beneficiary with payer secondary), placing them near symmetric: statehood and safety against identity strain and foregone practice. Devout believers, existing plural families, and performing priesthood holders sit near the full-target pole: declared victims with identity-locked or trapped exits, which amplifies effective extraction toward the ceiling. Colonists are the one correction: the derivation would read their mobile exit as damping extraction toward the beneficiary side, but their mobility was jurisdictional, partial, expensive, and finally reversed by revolution and extending discipline, so they bore near-believer costs; the override sets d=0.7 for the moderate atom (occupied in this story only by mexico_colony_settlers). The excluded seat contributes no directionality: authored absence is commentary-grade, never correction-grade.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding emergency (existential federal assault) was dead by roughly 1907, while the suspension persists through the interval end with no declared termination mechanism; the R5 pairing (dead founding problem x world_rearranges verdict) flags the zombie tendency honestly: the arrangement outlived its cause and hardened into a permanent norm. The temporal reading's own frame predicted its obsolescence ('when constraints lift'); the constraint demonstrably lifted and the suspension answered with the Second Manifesto instead of restoration, which is the frame failing on its own trigger (see omega restoration_condition_viability). Claiming tangled_rope prevents two mislabels: a pure-rope reading would hide the believer-side costs behind the genuine survival coordination, and a snare reading would erase the real existential collective-action problem the announcement solved and falsely imply the survival story was cover for rent-seeking. The late-period fate (calcified inertia, ceremonial doctrinal memory) seeds a piton-like trajectory beyond this interval, but within 1890-1930 the structure still coordinates and still extracts, so the claim stands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_committer_structure,
    'Which reading of the eternal_marriage_covenant kernel does this constraint instantiate, and what would adopting a sibling reading change structurally?',
    'Compare the sibling constraint files immutable_commandment_reading and prophetic_override_reading: sibling adoption swaps the victim set (the immutable reading makes non-practice itself the disobedience, raising epsilon on all believers; the override reading retroactively demotes the 1876 text to circumstance-bound, removing the double bind) and relocates enforcement authority (external coercion versus self-owned prophetic authority).',
    'This file instantiates temporal_accommodation_reading only. Its epsilon (0.58), victim set, and dual enforcement structure describe suspension-with-retained-doctrine. Under immutable_commandment_reading the same history computes at higher epsilon; under prophetic_override_reading epsilon drops and the bearing seats dissolve. Per-seat and story-level classifications are reading-indexed; averaging across sibling readings is prohibited.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel, with sibling deltas recorded.').

omega_variable(
    restoration_condition_viability,
    'Was the reading''s restoration condition (''when political constraints lift'') ever satisfiable, and did the arrangement answer it when satisfied?',
    'Historical test: federal pressure demonstrably lifted by roughly 1907 (Smoot seated after assurances, statutes unenforced thereafter). Observe whether suspension lifted. It did not: the 1904 Second Manifesto hardened it. The frame''s own trigger fired and went unanswered.',
    'If the restoration condition is ignored when satisfied, the reading''s temporal promise is void, the arrangement loses its transitional justification, and the structure hardens toward permanent prohibition, drifting this reading toward override-outcomes without override''s doctrinal honesty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(restoration_condition_viability, empirical, 'Whether the deferred-command frame''s own restoration trigger fired and went unanswered.').

omega_variable(
    enforcement_authority_locus,
    'Is the suspension enforced solely by external federal coercion, or does ecclesiastical enforcement (Second Manifesto, temple-interview questions, depositions) carry independent binding legitimacy of its own?',
    'Track enforcement behavior after external pressure ends: if discipline continues in the absence of federal threat, internal enforcement is self-owned; if it lapses, the external coercion was doing the work.',
    'External-only suppression keeps the arrangement accommodation-under-duress with lower owned suppression; self-owned enforcement converts the suspension into authoritative revision from within and raises the suppression attributable to the constraint itself rather than to the state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_authority_locus, empirical, 'Locus of enforcement authority over the suspension: borrowed coercion or self-owned discipline.').

omega_variable(
    existing_family_cost_attribution,
    'Do existing plural families'' costs (legal non-recognition, stigma, economic precarity) belong to this suspension arrangement or to the pre-existing federal statutes it responded to?',
    'Counterfactual attribution: costs persisting after 1890 that the arrangement had power to relieve but did not pursue (no amnesty or legalization effort for existing unions was ever mounted) count to the arrangement; costs the arrangement lacked any power to relieve count to the statutes.',
    'Full attribution pushes epsilon above 0.7 and the bearing seats toward full-target; statutory attribution drops the arrangement''s share below 0.5 and moves the structure toward rope. The referent is fixed (the standing suspension arrangement), but the cost ledger inside it is contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existing_family_cost_attribution, conceptual, 'Referent-boundary question for victim-cost accounting inside the fixed epsilon referent.').

omega_variable(
    dual_track_sincerity,
    'Was ''dormant pending restoration'' a sincere operational plan or rhetorical cover for intended-permanent abandonment?',
    'Private leadership correspondence and post-1904 doctrinal statements against the public framing; sincerity evidenced by whether any restoration pathway was maintained (sealing-authority instructions, colony support, records reserved for resumption). None is documented.',
    'A cover-story finding raises the structural share of theatricality and pushes the classification toward extraction riding a survival story; a sincere-dormancy finding supports the coordination reading and keeps the tangled_rope claim stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_track_sincerity, empirical, 'Sincerity of the temporality claim in the reading''s own operations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(eter_tr_t0, observed).
narrative_ontology:measurement(eter_tr_t5, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement_basis(eter_tr_t5, observed).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement_basis(eter_tr_t10, observed).
narrative_ontology:measurement(eter_tr_t14, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 14, 0.44).
narrative_ontology:measurement_basis(eter_tr_t14, observed).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(eter_tr_t20, observed).
narrative_ontology:measurement(eter_tr_t26, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 26, 0.3).
narrative_ontology:measurement_basis(eter_tr_t26, observed).
narrative_ontology:measurement(eter_tr_t33, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 33, 0.27).
narrative_ontology:measurement_basis(eter_tr_t33, observed).
narrative_ontology:measurement(eter_tr_t40, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(eter_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement_basis(eter_be_t0, observed).
narrative_ontology:measurement(eter_be_t5, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(eter_be_t5, observed).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(eter_be_t10, observed).
narrative_ontology:measurement(eter_be_t14, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 14, 0.68).
narrative_ontology:measurement_basis(eter_be_t14, observed).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(eter_be_t20, observed).
narrative_ontology:measurement(eter_be_t26, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 26, 0.63).
narrative_ontology:measurement_basis(eter_be_t26, observed).
narrative_ontology:measurement(eter_be_t33, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 33, 0.6).
narrative_ontology:measurement_basis(eter_be_t33, observed).
narrative_ontology:measurement(eter_be_t40, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(eter_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement_basis(eter_su_t0, observed).
narrative_ontology:measurement(eter_su_t5, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement_basis(eter_su_t5, observed).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(eter_su_t10, observed).
narrative_ontology:measurement(eter_su_t14, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 14, 0.76).
narrative_ontology:measurement_basis(eter_su_t14, observed).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(eter_su_t20, observed).
narrative_ontology:measurement(eter_su_t26, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 26, 0.68).
narrative_ontology:measurement_basis(eter_su_t26, observed).
narrative_ontology:measurement(eter_su_t33, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 33, 0.64).
narrative_ontology:measurement_basis(eter_su_t33, observed).
narrative_ontology:measurement(eter_su_t40, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(eter_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, prophetic_override_reading).

% DUAL FORMULATION NOTE:
% Colloquial label 'the 1890 Manifesto' decomposes into three structurally distinct constraints sharing one canonized kernel (D&C 132). Epsilon differs sharply across readings: the immutable reading withholds a practice it holds eternally owed (highest extraction on believers); the temporal reading (this file) imposes a suspension double-bind at mid extraction; the override reading resolves the tension authoritatively (lowest extraction, no double bind). Family structure: the immutable reading is the upstream textual claim (D&C 132's own universality), cited by the temporal reading as the reason the doctrine persists; the override reading is the downstream absorption of the resolution the temporal reading refuses. All three stories link one another via affects_constraints; each carries a single stable epsilon per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
