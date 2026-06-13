% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__first_holding_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: Author Entry Into IP Occupancy (First Holding Reading, 1710)
 *   domain: legal/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   The Statute of Anne (1710) creates a fundamental occupancy change in
 *   literary property law: authors enter the legitimate claimant set as
 *   statutory rightholders, displacing the Stationers' Company's prior
 *   monopsony. This reading (first_holding_reading) tracks the membership
 *   shift itself—WHO is recognized as a valid property-claimant—as the
 *   decisive constraint structural change. The statute replaces Stationers'
 *   corporate assignment monopoly with distributed author claims under
 *   statutory term. Pre-1710, authors had no direct suit capacity; post-1710,
 *   they do. The constraint's core is the occupancy reallocation, not the
 *   prior question of whether ownable expression was thinkable (that is the
 *   thinkability_reading sibling). This reading is independent in ε: it
 *   measures the extraction cost of the occupancy shift itself, not the
 *   pre-existing thinkability state.
 *
 * KEY AGENTS:
 *   - authors_as_statutory_rightholders: Entry beneficiary (gain direct claim capacity, statutory term protection, assignability)
 *   - stationers_monopoly_incumbent: Primary payer (lose monopsony control, must compete with authors for assignee interest)
 *   - parliamentary_statutory_authority: Agenda-setter (enacts statute, enforces the occupancy reallocation)
 *   - printers_booksellers_small: Secondary beneficiary (gain competitive entry via direct author contracts)
 *   - readers_print_consumers: Beneficiary with minor cost (term limit enables commons entry faster than perpetual monopoly)
 *   - crown_prerogative_claimants: Excluded (discretionary license authority displaced by statute)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.48).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.62).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Author Entry Into IP Occupancy (First Holding Reading, 1710)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, 'bd816995-df27-449b-9594-b4cff2a85ba0').
narrative_ontology:cs_kernel_codification('bd816995-df27-449b-9594-b4cff2a85ba0', formalized).
narrative_ontology:cs_authority_grounding('bd816995-df27-449b-9594-b4cff2a85ba0', extraction).
narrative_ontology:cs_interpretation_layer_present('bd816995-df27-449b-9594-b4cff2a85ba0').
narrative_ontology:cs_reading_relation('bd816995-df27-449b-9594-b4cff2a85ba0', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('bd816995-df27-449b-9594-b4cff2a85ba0', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('bd816995-df27-449b-9594-b4cff2a85ba0', foundational, occupancy_as_primary_structural_change).
narrative_ontology:cs_axiom_status(occupancy_as_primary_structural_change, holdable).
narrative_ontology:cs_axiom_grounding('bd816995-df27-449b-9594-b4cff2a85ba0', occupancy_as_primary_structural_change, conventional).
narrative_ontology:cs_axiom('bd816995-df27-449b-9594-b4cff2a85ba0', foundational, statutory_membership_determination_over_natural_coherence).
narrative_ontology:cs_axiom_status(statutory_membership_determination_over_natural_coherence, holdable).
narrative_ontology:cs_axiom_grounding('bd816995-df27-449b-9594-b4cff2a85ba0', statutory_membership_determination_over_natural_coherence, conventional).
narrative_ontology:cs_reference_frame('bd816995-df27-449b-9594-b4cff2a85ba0', stationers_corporate_monopoly).
narrative_ontology:cs_drift_state('bd816995-df27-449b-9594-b4cff2a85ba0', statute_of_anne_enactment, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('bd816995-df27-449b-9594-b4cff2a85ba0', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, authors_as_statutory_rightholders).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_monopoly_incumbent).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, printers_booksellers_small).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, readers_print_consumers).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, author_natural_right_doctrine).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, literary_property_statutory_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enter the legitimate claimant set for literary property rights through the Statute of Anne (1710). Prior to this date, authors had no direct property claim; rights flowed through Stationers' Company assignments. Post-1710, authors can claim statutory term protection and assign rights themselves, displacing the Stationers as sole intermediate. The entry creates a new seat at the enforcement table.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, authors_as_statutory_rightholders, beneficiary,
    moderate, generational, constrained, national).

% The Stationers' Company loses monopoly control over literary property when the author seat is created. They previously extracted rents as sole interface between authors and readers—ownership flowed through their corporate grants. The 1710 reallocation embeds authors as direct rightholders, fragmenting their prior monopsony over assignment. They must now compete with authors for assignee interest.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_monopoly_incumbent, payer,
    institutional, generational, trapped, national).

% Parliament enacts the Statute of Anne, creating the new author seat via statutory law. The statute is the enforcement instrument that declares authors eligible rightholders and sets the term (14 years renewable once). Parliament enforces the statute through court recognition of author suits and Stationers' monopoly dissolution.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, parliamentary_statutory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the collapse of the Stationers' monopoly because they can now contract directly with authors rather than petitioning the Stationers for participation. This creates competitive entry points for printing and distribution. The author seat's creation enables them to sidestep the corporate licensing bottleneck.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, printers_booksellers_small, beneficiary,
    moderate, biographical, constrained, regional).

% Benefit from the term limit (14 years) because monopoly control is bounded; works enter the commons faster than perpetual Stationers' assignments would allow. They also benefit from increased competitive entry among printers. The cost is the temporary exclusivity they must honor during the term.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, readers_print_consumers, beneficiary,
    powerless, biographical, constrained, national).

% The Crown's historic licensing prerogative and private monopoly grants (the basis of the Stationers' privilege) are structurally excluded from the new statutory regime. They would have argued for continued discretionary grant authority; the statute cuts off that mechanism in favor of statutory-term allocation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, crown_prerogative_claimants, excluded,
    institutional, generational, trapped, national).

% Observe and debate whether the statute creates author rights by statute (positive law) or recognizes pre-existing natural rights (property in labor). This reading (first holding) treats the statute as the decisive membership-set change, independent of the thinkability question of whether natural author rights were coherent pre-1710.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, legal_philosophers_natural_rights, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, parliamentary_statutory_authority).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates literary property rights systematically: replaces ad-hoc Stationers' corporate patronage with a statutory rule-set granting authors direct claim for a defined term, enabling competitive entry and publishing specialization.
% TRANSFER_FUNCTION: Transfers the occupancy of the literary property seat from the Stationers' Company (institutional monopolist, sole legitimate claimant) to authors (distributed individual claimants with statutory term). The Stationers lose monopsony control; authors gain direct suit-capacity and assignability.
% ABSENT_VOICES: The Crown's prerogative licensing interests are excluded from the statutory regime—they would have argued for discretionary grant continuity rather than statutory allocation. Competing literary guilds (continental privilege systems) are excluded from the English statutory framework debate; their alternative occupancy models do not enter the English legislative record.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne and the author-seat creation vanished overnight, literary property would revert to Stationers' monopoly assignment (or Crown licensing), authors would lose direct claim capacity, and the publishing market would consolidate around institutional gatekeepers rather than distributed author bargains. The occupancy change is constitutive of the modern author-publisher relationship.
% FOUNDING_PROBLEM: The Stationers' Company monopoly extracted monopoly rents from authors and readers by controlling the sole legitimate claimant seat. Authors had no direct property claim; their work flowed through corporate assignment. Readers faced perpetual copyright from corporate monopoly control (no term limit). The printing market could not diversify because entry required Stationers' permission.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary parliamentary debates (1710) attest the monopoly problem; legal historians (outside the beneficiary seat) confirm the Stationers' monopsony control and its rent-extraction effects. The statute's preamble itself cites the monopoly as the problem. Post-1710 legal records show competitive entry among printers and author-initiated litigation, confirming the occupancy shift persisted.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness drops from 0.72 (pre-1710 monopoly) to 0.48 (post-statute) because the occupancy shift fragments the monopsony: authors can now negotiate directly with printers, and the term limit (14 years renewable) creates a pathway to the commons. Suppression requirement drops from 0.78 to 0.62 because maintaining the monopoly required constant corporate enforcement; the statute replaces enforcement with rule-based allocation, reducing active suppression need. Theater rises from 0.18 to 0.41 post-1710 because the statutory framework requires continuous interpretation disputes (term duration debates, remedies for infringement, the scope of derivative works)—the enforcement machinery becomes more performative relative to the underlying allocation rule. The occupancy shift itself is sharp (1710 is the boundary); measurements before and after show the constraint transitioning. This reading claims tangled_rope: genuine coordination function (systematic copyright allocation) paired with asymmetric extraction (the Stationers bear the cost of occupancy loss while others benefit from competitive entry and term limits).
 *
 * PERSPECTIVAL GAP:
 *   From the legal-philosophy observer seat, the key divergence is between the first_holding_reading (occupancy as empirical fact of recognized membership) and the thinkability_reading (occupancy as proof of category coherence). These are not the same claim. First holding says: 'In 1710, authors entered the legitimate claimant set because Parliament said so.' Thinkability says: 'Authors could not be property-claimants until ownable expression became thinkable as a category.' The first reading is about who-gets-to-claim (occupancy membership), the second about what-can-be-owned (category emergence). A jurisdiction might admit thinkability without first holding (authors were always thinkable but weren't admitted until 1710 for political reasons); or first holding without thinkability (the statute creates a fictional member who was never coherent as a rights-bearer). This reading commits to the occupancy reading and treats thinkability as a separate, sibling constraint with a different ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors enter with d near 0.3–0.4 (they gain new claim capacity but remain structurally constrained by statute and assignee behavior). The Stationers drop to high d (0.8+): they lose monopoly control, must now compete, and face legal displacement. The directionality reversal (authors from zero to beneficiary, Stationers from agenda-setter to payer) is the occupancy shift itself. Parliamentary authority sits at d~0.5 (neutral implementer, neither collecting nor paying). Small printers gain moderately (d~0.35: they benefit from competitive entry but remain dependent on author/assignee negotiation). This reading does NOT use directionality_overrides because the structural derivation from beneficiary/victim + exit already captures the seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Stationers' monopsony extraction) remains live post-1710 in contested form. The statute does not abolish Stationer intermediation; it adds authors as legitimate direct claimants. The intermediate problem (whether authors retain long-term seat stability, or whether re-monopolization threatens) is live. The constraint classifies as tangled_rope rather than pure rope because the occupancy shift is enforced (Parliament had to act, courts had to recognize author suits, the Stationers had to be barred from asserting perpetual rights) and extraction persists (the statutory term is itself a limit that preserves monopoly control for 14 years). The constraint is not a snare because it creates genuine coordination (distributed claims enable specialization) alongside asymmetric extraction (the prior monopsony holder loses). Mandatrophy would arise if the statute's occupancy function ceased (if courts ceased recognizing author suits, or if re-monopolization occurred), but at interval end (1750) the constraint remains mandated and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    occupancy_vs_thinkability_independence,
    'Are occupancy (first holding) and thinkability (category coherence) formally independent, or is one a logical precondition of the other?',
    'Comparative legal history: did any jurisdiction recognize authors as legitimate claimants before thinkability was established? Or was thinkability necessary for occupancy? Evidence from continental systems (France, Netherlands) that enacted author protections at different times with different justifications.',
    'If independent: two genuinely distinct constraints with different ε values (first_holding_reading and thinkability_reading both live). If occupancy presupposes thinkability: first_holding is a manifestation of thinkability, not a separate structural change; reclassify to single-constraint reading. If thinkability presupposes occupancy: reverse the dependency. If neither presupposes the other but they are temporally entangled: evidence for the synchronic_diachronic_seam reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupancy_vs_thinkability_independence, conceptual, 'Whether occupancy-shift and category-emergence are structurally independent constraint claims.').

omega_variable(
    statutory_vs_natural_right_grounding,
    'Does the Statute of Anne CREATE author property rights (positive law positing new membership), or RECOGNIZE pre-existing natural author rights (discovering what was always coherent)?',
    'Parliamentary debate record (1710) and pre-1710 legal writing: did petitioners argue for author rights on natural-right grounds (labor in expression) or pragmatic grounds (monopoly remedy)? Post-1710 case law and doctrinal commentary: did judges treat the statute as creative or recognitional?',
    'If creative: the occupancy reallocation is the primary structural change; first_holding_reading captures it. If recognitional: the statute is evidence of prior thinkability; thinkability_reading is the deeper constraint. If mixed/contested: the grounding ambiguity belongs in this omega, not in the axiom set (axioms declare what THIS reading commits to; the ambiguity is about the reading''s justification, not its content).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_vs_natural_right_grounding, conceptual, 'Whether statutory authority creates or recognizes author rights.').

omega_variable(
    stationers_actual_exit,
    'Did the Stationers'' Company actually remain trapped post-1710, or did they adapt by transitioning to a new role (assignee, agent, publisher) and gain mobility within the transformed market?',
    'Historical record of Stationers'' Company activity post-1710: did membership decline, firms exit, or did the corporate form persist by shifting function? Estate records, guild membership rolls, printing ledgers showing whether Stationers remained institutional gatekeepers or became competitive service-providers.',
    'If actually trapped: the victim classification holds; high d for the Stationers. If adapted/mobile: the exit_options should reclassify from trapped to constrained or mobile; directionality shifts downward (less target-like). If the Company dissolved: the occupancy reallocation was more complete than the constraint models; the payer seat vanished.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stationers_actual_exit, empirical, 'Whether the Stationers'' institutional position was permanently displaced or functionally adapted.').

omega_variable(
    term_limit_as_compromise_vs_principle,
    'Is the 14-year term limit a principled limit on author exclusivity (authors deserve finite monopoly, then commons), or a political compromise between author advocates and commons defenders (pragmatic middle ground)?',
    'Parliamentary debate; doctrinal justification in post-1710 legal writing; whether later statutory extensions (1814, 1886, 1976) were framed as principled expansions or as accumulated compromises. If principled: why that term length? What justification was offered?',
    'If principled: the constraint''s extraction limit is foundationally justified; the occupancy shift includes a built-in sunset (cooperative/scaffold element). If compromise: the term limit is a contested boundary, and the constraint risks extension cycles (oscillating extraction); theater and suppression might spike around renewal debates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(term_limit_as_compromise_vs_principle, preference, 'Whether term limits are principled or contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1700, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_first_holding_tr_t1700, ip_category_emergence__first_holding_reading, theater_ratio, 1700, 0.18).
narrative_ontology:measurement_basis(ip_first_holding_tr_t1700, observed).
narrative_ontology:measurement(ip_first_holding_tr_t1708, ip_category_emergence__first_holding_reading, theater_ratio, 1708, 0.22).
narrative_ontology:measurement_basis(ip_first_holding_tr_t1708, observed).
narrative_ontology:measurement(ip_first_holding_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.41).
narrative_ontology:measurement_basis(ip_first_holding_tr_t1710, observed).
narrative_ontology:measurement(ip_first_holding_tr_t1720, ip_category_emergence__first_holding_reading, theater_ratio, 1720, 0.45).
narrative_ontology:measurement_basis(ip_first_holding_tr_t1720, observed).
narrative_ontology:measurement(ip_first_holding_tr_t1735, ip_category_emergence__first_holding_reading, theater_ratio, 1735, 0.42).
narrative_ontology:measurement_basis(ip_first_holding_tr_t1735, observed).
narrative_ontology:measurement(ip_first_holding_tr_t1750, ip_category_emergence__first_holding_reading, theater_ratio, 1750, 0.41).
narrative_ontology:measurement_basis(ip_first_holding_tr_t1750, observed).

% Extraction over time
narrative_ontology:measurement(ip_first_holding_be_t1700, ip_category_emergence__first_holding_reading, base_extractiveness, 1700, 0.72).
narrative_ontology:measurement_basis(ip_first_holding_be_t1700, observed).
narrative_ontology:measurement(ip_first_holding_be_t1708, ip_category_emergence__first_holding_reading, base_extractiveness, 1708, 0.7).
narrative_ontology:measurement_basis(ip_first_holding_be_t1708, observed).
narrative_ontology:measurement(ip_first_holding_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.48).
narrative_ontology:measurement_basis(ip_first_holding_be_t1710, observed).
narrative_ontology:measurement(ip_first_holding_be_t1720, ip_category_emergence__first_holding_reading, base_extractiveness, 1720, 0.45).
narrative_ontology:measurement_basis(ip_first_holding_be_t1720, observed).
narrative_ontology:measurement(ip_first_holding_be_t1735, ip_category_emergence__first_holding_reading, base_extractiveness, 1735, 0.48).
narrative_ontology:measurement_basis(ip_first_holding_be_t1735, observed).
narrative_ontology:measurement(ip_first_holding_be_t1750, ip_category_emergence__first_holding_reading, base_extractiveness, 1750, 0.48).
narrative_ontology:measurement_basis(ip_first_holding_be_t1750, observed).

% Suppression requirement over time
narrative_ontology:measurement(ip_first_holding_su_t1700, ip_category_emergence__first_holding_reading, suppression_requirement, 1700, 0.78).
narrative_ontology:measurement_basis(ip_first_holding_su_t1700, observed).
narrative_ontology:measurement(ip_first_holding_su_t1708, ip_category_emergence__first_holding_reading, suppression_requirement, 1708, 0.76).
narrative_ontology:measurement_basis(ip_first_holding_su_t1708, observed).
narrative_ontology:measurement(ip_first_holding_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.62).
narrative_ontology:measurement_basis(ip_first_holding_su_t1710, observed).
narrative_ontology:measurement(ip_first_holding_su_t1720, ip_category_emergence__first_holding_reading, suppression_requirement, 1720, 0.61).
narrative_ontology:measurement_basis(ip_first_holding_su_t1720, observed).
narrative_ontology:measurement(ip_first_holding_su_t1735, ip_category_emergence__first_holding_reading, suppression_requirement, 1735, 0.62).
narrative_ontology:measurement_basis(ip_first_holding_su_t1735, observed).
narrative_ontology:measurement(ip_first_holding_su_t1750, ip_category_emergence__first_holding_reading, suppression_requirement, 1750, 0.62).
narrative_ontology:measurement_basis(ip_first_holding_su_t1750, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__first_holding_reading, 0.12).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% The ip_category_emergence kernel splits into three sibling constraint readings: (1) first_holding_reading (this file): occupancy reallocation—authors enter legitimate claimant set, Stationers lose monopsony. (2) thinkability_reading: category emergence—ownable expression becomes legally coherent. (3) synchronic_diachronic_seam: formal question whether thinkability and first_holding are independent or temporal artifacts. Each reading has its own ε, beneficiary/victim structure, and type. first_holding_reading influences (but does not foreclose) the thinkability reading: occupancy provides evidence for thinkability, but occupancy could occur without thinkability being live (Parliament could create members of an incoherent category). thinkability_reading influences first_holding: a thinkable category makes occupancy coherent, but thinkability alone does not guarantee occupancy (Parliament could have left authors unoccupied). No reading forecloses the others: all three remain live interpretive claims. The synchronic_diachronic_seam coexists with both substantive readings and tracks the M4/M5 collapse question (are the readings time-indexed variants of one structure, or genuinely distinct). All three files must link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
