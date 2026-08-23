% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Canonical-Lineage Marriage Authority (Christian Personal Law Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   Under this reading of the marriage_authority_kernel, legitimate authority
 *   over the formation and dissolution of Christian marriages in India flows
 *   through canonical transmission codified in statute: the Indian Christian
 *   Marriage Act 1872 governs solemnization and registration, with the Indian
 *   Divorce Act 1869 (as amended, most consequentially in 2001) governing
 *   dissolution. The arrangement coordinates a real collective problem —
 *   dozens of denominations with divergent customs, unified into one legally
 *   legible marriage frame — while extracting asymmetrically through a
 *   fault-based dissolution regime whose burdens fell hardest on wives and on
 *   spouses without documentary proof. The claim/metric gap is deliberate and
 *   independent: claimed_type is authored from my structural belief (a
 *   genuine coordination function fused with enforced asymmetric extraction,
 *   hence tangled_rope), while the metrics describe the arrangement's actual
 *   operation as the record shows it; the engine computes per-seat types from
 *   the structural data, and divergence between my claim and any computed
 *   seat is signal, not error. KEY AGENTS (by structural relationship): -
 *   episcopal_hierarchies: agenda-setter and concentrated beneficiary
 *   (institutional / arbitrage) — administers canonical jurisdiction and
 *   collects continuity of authority - parochial_clergy_officiants:
 *   beneficiary (moderate / constrained) — collects standing from gatekeeping
 *   solemnization - christian_laity_communities: beneficiary with payer side
 *   (organized / identity_locked) — receives sacramental coordination,
 *   absorbs stalled households - wives_trapped_by_fault_grounds: primary
 *   target (moderate / trapped) - deserted_spouses_without_proof: target and
 *   coalition candidate (powerless / trapped) -
 *   christian_womens_reform_organizations: excluded voice (organized /
 *   constrained) — drove the 2001 reform from outside the canonical forum -
 *   indian_legislature: co-agenda-setter bearing legitimacy costs
 *   (institutional / constrained) - constitutional_courts: adjudicating
 *   observer (institutional / analytical) - legal_pluralism_scholars:
 *   analytical observer
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.55).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.44).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Canonical-Lineage Marriage Authority (Christian Personal Law Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '6abc4e11-48aa-4895-a725-10239608fc74').
narrative_ontology:cs_kernel_codification('6abc4e11-48aa-4895-a725-10239608fc74', fixed_text).
narrative_ontology:cs_authority_grounding('6abc4e11-48aa-4895-a725-10239608fc74', lineage).
narrative_ontology:cs_interpretation_layer_present('6abc4e11-48aa-4895-a725-10239608fc74').
narrative_ontology:cs_reading_relation('6abc4e11-48aa-4895-a725-10239608fc74', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('6abc4e11-48aa-4895-a725-10239608fc74', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6abc4e11-48aa-4895-a725-10239608fc74', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('6abc4e11-48aa-4895-a725-10239608fc74', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('6abc4e11-48aa-4895-a725-10239608fc74', foundational, marriage_authority_flows_through_canonical_transmission).
narrative_ontology:cs_axiom_status(marriage_authority_flows_through_canonical_transmission, holdable).
narrative_ontology:cs_axiom_grounding('6abc4e11-48aa-4895-a725-10239608fc74', marriage_authority_flows_through_canonical_transmission, theological).
narrative_ontology:cs_axiom('6abc4e11-48aa-4895-a725-10239608fc74', foundational, bond_dissolvable_only_on_proved_cause).
narrative_ontology:cs_axiom_status(bond_dissolvable_only_on_proved_cause, holdable).
narrative_ontology:cs_axiom_grounding('6abc4e11-48aa-4895-a725-10239608fc74', bond_dissolvable_only_on_proved_cause, deontological).
narrative_ontology:cs_reference_frame('6abc4e11-48aa-4895-a725-10239608fc74', codified_canonical_jurisdiction).
narrative_ontology:cs_drift_state('6abc4e11-48aa-4895-a725-10239608fc74', post_2001_amendment_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6abc4e11-48aa-4895-a725-10239608fc74', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, episcopal_hierarchies).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, parochial_clergy_officiants).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_laity_communities).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, wives_trapped_by_fault_grounds).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, deserted_spouses_without_proof).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_laity_communities).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, indian_legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Synods, bishops, and diocesan matrimonial tribunals define who may marry, how unions are recorded, and on what terms a bond may be declared null. They collect continuity of jurisdiction — marriages solemnized under their forms remain theirs to interpret — and operate a dual track in which a church nullity decree and a civil decree answer different audiences. Their change lever is canonical reinterpretation and lobbying over the statute; their exit would mean ceding marriage governance altogether, dissolving the pastoral office that depends on it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, episcopal_hierarchies, agenda_setter,
    institutional, generational, arbitrage, national).

% Parish priests solemnize marriages under the Act's forms, maintain registers, and are usually a couple's first contact with both civil and canonical process. Standing, livelihood, and congregational authority are tied to being the gate of valid union; stepping back from that gatekeeping would mean abandoning the practical core of the office.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, parochial_clergy_officiants, beneficiary,
    moderate, biographical, constrained, local).

% Congregants receive a coherent sacramental frame: recognized marriage, communal celebration, uncontested legitimacy of children, a shared vocabulary of marital obligation. They pay when a member's dead marriage cannot be dissolved — the community absorbs the stalled household — and they pay again when reform arrives framed as an attack on faith identity. Leaving would mean leaving the community of worship itself, which most experience as self-annihilation rather than a menu option.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_laity_communities, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, christian_laity_communities, payer).

% Wives in dead marriages under the fault regime must assemble documentary proof of adultery compounded by cruelty or desertion to petition for dissolution, and before 2001 faced grounds formally harsher than a husband's. Without proof, neither a civil decree nor a church nullity moves; separation carries stigma and economic exposure, and for Catholics remarriage after divorce means exclusion from communion. Exit exists on paper as litigation few can fund or evidence.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, wives_trapped_by_fault_grounds, payer,
    moderate, biographical, trapped, regional).

% Spouses abandoned by partners who left no provable trail — migrant workers, informal separations, vanished spouses — hold no evidentiary basis for any petition. The remedies presuppose documents and witnesses that poverty and migration erase. Individually they surface mainly in casework and advocacy statistics rather than courtrooms; their realistic channel is collective action through the reform organizations that took up their cases.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, deserted_spouses_without_proof, payer,
    powerless, biographical, trapped, regional).

% Feminist and church-reform networks documented stalled-marriage caseloads through the 1980s and 1990s and ran the campaign that produced the 2001 amendment. They sit outside the tribunals and synods where dissolution authority is exercised, reaching the system through legislatures and courts rather than the canonical forum itself.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_womens_reform_organizations, excluded,
    organized, generational, constrained, national).

% Parliament codified canonical norms into the 1872 statute and retains amendment power, exercised in the 2001 Divorce (Amendment) Act that equalized grounds and added mutual-consent dissolution after separation. Every intervention trades religious-freedom guarantees against equality commitments and draws counter-mobilization from community leadership; the legislature bears legitimacy costs in both directions and cannot exit the pluralism it administers.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_legislature, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, indian_legislature, payer).

% High Courts and the Supreme Court adjudicate challenges to the personal-law settlement, weighing religious-freedom protections against equality guarantees. They take testimony from every other seat, commission comparative analysis, and can strike or reshape enforcement, but they deliberate rather than administer.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Comparative-law researchers map how the five readings partition the population, trace the colonial genealogy of the 1872 codification, and publish analyses that reform coalitions and courts cite. Analytically outside the arrangement; nothing flows to or from them under it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, legal_pluralism_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, episcopal_hierarchies).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one authoritative, statutorily backed frame for solemnizing and registering Christian marriages across many denominations and regions, so that validity, the legitimacy of offspring, and inheritance claims are settled by a single recognizable authority instead of being renegotiated parish by parish or couple by couple.
% TRANSFER_FUNCTION: Moves jurisdiction over marital formation and dissolution from couples to episcopal and statutory-canonical institutions. Within dissolution specifically, it has historically moved exit-bargaining power toward spouses who control evidence and institutional standing, and away from wives and spouses without documentary proof.
% ABSENT_VOICES: Wives inside stalled marriages without litigation resources, deserted spouses erased from the documentary record, and Christians outside the recognized churches would object loudest. They are absent from the synods and tribunals where dissolution authority lives, reaching the system only through legislatures, courts, and advocacy organizations that speak for them at one remove.
% DISAPPEARANCE_RATIONALE: If the canonical-lineage arrangement vanished overnight, every pending dissolution, nullity, and solemnization would need a successor authority: civil courts would absorb the caseload, church sacramental life would lose its legal anchor and reorganize around purely voluntary canonical observance, and the five-reading partition of the population would collapse into whichever single regime replaced it — a wholesale rearrangement of who governs marriage for millions of people.
% FOUNDING_PROBLEM: Colonial administration confronted dozens of Christian denominations with incompatible marriage customs, no reliable registry, and chronic disputes over the legitimacy of children and the devolution of property; the 1872 Act imposed one statutory frame modeled on canonical norms to make Christian marriages uniformly recordable and legally legible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Law Commission of India's reviews of the Christian marriage and divorce statutes, the parliamentary record around the 2001 Divorce (Amendment) Act, and academic legal histories of colonial family law all attest that the registration-legitimacy problems were substantially solved while the dissolution-authority question stayed disputed. Church submissions to the same consultations attest the opposite emphasis — which is exactly why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55: dissolution under fault grounds requires proof many cannot assemble, and until 2001 the grounds were formally harsher for wives than husbands — a decoupling of exit price from the reality of a dead marriage that is the arrangement's core extraction. Suppression 0.44 (raw structural property, deliberately unscaled by power or scope; the engine scales only extractiveness): alternatives exist — civil solemnization under the Special Marriage Act, relocation, litigation — but each carries identity or cost penalties, so alternatives are suppressed rather than absent. Theater_ratio 0.34 and rising: the nullity track increasingly performs jurisdiction that civil courts actually exercise, while registration and solemnization remain fully functional. Accessibility_collapse 0.45: understanding the arrangement does not close alternatives the way a natural law would — a determined couple can exit the frame entirely — so collapse stays moderate. Resistance 0.55: organized campaigns (Joint Women's Programme, Streevani and allied networks), parliamentary pressure, and constitutional litigation produced the 2001 amendment; resistance is real and periodically efficacious. The measurement series run on ONE shared nine-point grid (every tracked metric authored at every point) because enforcement capacity genuinely changed over the interval: colonial-era build-up of the statutory-canonical machinery, a long ratchet while peer regimes liberalized around it, then a step-down after the 2001 amendment equalized grounds and introduced mutual-consent dissolution — that is an enforcement-capacity trajectory, which is why suppression_requirement is tracked here rather than left to the static scalar. The trajectory is monotone-rising then discontinuous-falling, not cyclical; the 2001 break at approximately t=129 lands between the t=120 and t=133 points, which is why those two points carry the largest deltas in the series.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the episcopal seat the arrangement is covenant stewardship the church built and defends — genuine coordination it administers. From the trapped-wife seat the same structure is a locked door whose lock is her missing paperwork. The laity seat is split down the middle by identity fusion: the relational and institutional identity of belonging — sacramental marriage, communion standing, communal recognition — is constituted through the very arrangement whose dissolution rules can strand a member, which is why the laity carry identity_locked exit and why reform proposals arrive experienced as attacks on faith itself. The legislature experiences the arrangement as a standing constitutional trade-off it cannot lay down; the courts as a rights-versus-community adjudication; the scholars as an object lesson in pluralist partition. The engine derives this divergence from the structural data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations pull the episcopal and clerical seats toward the subsidy end (low d): they collect jurisdiction, standing, and continuity, with the hierarchies additionally able to arbitrage between the canonical and civil tracks. Victim declarations push the two trapped spouse seats toward the full-target end (high d), amplified by trapped exit — no arbitrage-grade exit anywhere in the payer set. The laity sit near symmetric: declared beneficiaries receiving real coordination benefit, but identity_locked exit and absorbed costs of stalled households pull the other way. The legislature is a genuinely dual-positioned agent — it administers and could amend the arrangement (agenda-setter) yet pays legitimacy costs on both flanks (payer secondary role) — landing it well above the hierarchies' d despite sharing the institutional power atom. No directionality_overrides are authored: the exit-option differentiation (arbitrage vs constrained vs trapped vs identity_locked) plus the secondary_role declarations already separate the seats the derivation would otherwise blur, and overriding on a shared power atom would misapply one d value to structurally distinct agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — colonial-era chaos of unregistered marriages, disputed legitimacy of children, and uncertain property devolution across dozens of denominations — is substantially solved by the modern civil-documentation state independent of this arrangement. What persists is not that problem but the authority question: who governs dissolution. The classification prevents two opposite errors. Labeling the whole thing a snare erases the real coordination function (sacramental coherence, uniform registration) that the laity visibly still use; labeling it a rope whitewashes the enforced asymmetry in dissolution access that trapped spouses bear. Tangled rope holds both halves. The piton risk is real but localized: the rising theater_ratio series tracks the annulment track drifting toward performance while civil courts do the operative work — a component-level vestigialization the tribunal omega isolates for separate resolution rather than letting it contaminate the whole-story classification. Fixing_cost is authored prohibitive because the actors who could fix it (the legislature) face costs — constitutional religious-freedom trade-offs, community counter-mobilization, uniform-civil-code entanglement — that exceed any marginal benefit relative to the status quo's manageable friction, as the decade-long gap between the Law Commission's recommendations and the 2001 amendment demonstrates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_authority_source_disagreement,
    'Where exactly do the five readings of the marriage_authority_kernel locate legitimate authority over marriage, and which structural element does this reading''s canonical-transmission claim displace?',
    'Cross-reading comparison once the sibling stories are compiled: contrast each sibling''s axioms and reference_frame against this one''s; the displacement locus is whichever axiom set no single framework can jointly hold.',
    'Determines which sibling relations are foreclosing versus merely competitive. If the source-of-authority element is the only axis of disagreement, all four siblings remain coexistable and the corpus models stable pluralism rather than escalating contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_authority_source_disagreement, conceptual, 'Committer structure routed per Rule 2: this constraint is the christian_canonical_reading of the marriage_authority_kernel; the disagreement among readings is located in the source-of-authority element, not in the content of marriage rules themselves.').

omega_variable(
    canonical_persistence_vs_preference,
    'Does the canonical reading persist because Christian communities prefer canonical governance of marriage, or because statutory lock-in and church discipline suppress the alternatives?',
    'Revealed-preference data: uptake of Special Marriage Act solemnization among Christians, demand for civil-only dissolution channels, and survey evidence on preferred reform directions.',
    'If preference-driven, the arrangement sits closer to consent-based coordination and effective extraction falls; if lock-in-driven, effective extraction rises and the payer seats harden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_persistence_vs_preference, empirical, 'Whether persistence reflects community endorsement or enforcement lock-in.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the suppression holding stalled-marriage spouses in place structural (statutory fault grounds, tribunal procedure, documentary prerequisites) or internalized (the indissolubility doctrine making exit unthinkable before any legal barrier binds)?',
    'Post-exit trajectory: compare spouses who pursued civil dissolution despite doctrinal objection with demographically similar spouses who did not; persistence of self-constraint after the legal barrier falls indicates the internalized share.',
    'If substantially internalized, the scalar suppression measure understates the constraint''s grip — targets carry it past legal exit — and effective suppression for identity-locked seats rises above the authored value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Suppression mechanism split: roughly structural-majority by casework appearance, but the internalized share is unmeasured and materially changes per-seat classification.').

omega_variable(
    tribunal_nullity_function_or_theater,
    'Do church matrimonial tribunals provide a functioning alternative dissolution route, or has the nullity track become performative maintenance running alongside civil supremacy?',
    'Tribunal caseload and outcome data compared against civil divorce statistics for the same population, plus structured interviews with diocesan judges on how decrees are used after issuance.',
    'A functioning nullity track lowers effective extraction by supplying a real second exit; a theatrical track raises theater_ratio and supports piton-drift hypotheses localized to the annulment component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tribunal_nullity_function_or_theater, empirical, 'Whether the annulment tribunals are a genuine second exit door or ceremony beside a closed one.').

omega_variable(
    gender_equity_delta_depth,
    'Did the 2001 equalization of divorce grounds produce substantive gender equity, or formal parity that masks procedural burdens (proof standards, court costs, witness availability) still falling disproportionately on wives?',
    'Post-2001 litigation outcomes disaggregated by petitioner sex, together with analysis of evidence-burden and cost profiles across ground types.',
    'Substantive equity softens the victims'' directionality and pulls epsilon down over time; formal-only parity leaves the victim structure intact and keeps the asymmetric-extraction half of the hybrid live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_delta_depth, empirical, 'Depth of the expected moderate-gender-equity structural delta for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 153).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement_basis(marr_tr_t40, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(marr_tr_t60, observed).
narrative_ontology:measurement(marr_tr_t80, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement_basis(marr_tr_t80, observed).
narrative_ontology:measurement(marr_tr_t100, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(marr_tr_t100, observed).
narrative_ontology:measurement(marr_tr_t120, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 120, 0.31).
narrative_ontology:measurement_basis(marr_tr_t120, observed).
narrative_ontology:measurement(marr_tr_t133, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 133, 0.33).
narrative_ontology:measurement_basis(marr_tr_t133, observed).
narrative_ontology:measurement(marr_tr_t153, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 153, 0.34).
narrative_ontology:measurement_basis(marr_tr_t153, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(marr_be_t40, observed).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(marr_be_t60, observed).
narrative_ontology:measurement(marr_be_t80, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 80, 0.61).
narrative_ontology:measurement_basis(marr_be_t80, observed).
narrative_ontology:measurement(marr_be_t100, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 100, 0.65).
narrative_ontology:measurement_basis(marr_be_t100, observed).
narrative_ontology:measurement(marr_be_t120, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 120, 0.68).
narrative_ontology:measurement_basis(marr_be_t120, observed).
narrative_ontology:measurement(marr_be_t133, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 133, 0.57).
narrative_ontology:measurement_basis(marr_be_t133, observed).
narrative_ontology:measurement(marr_be_t153, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 153, 0.55).
narrative_ontology:measurement_basis(marr_be_t153, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement_basis(marr_su_t40, observed).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement_basis(marr_su_t60, observed).
narrative_ontology:measurement(marr_su_t80, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 80, 0.57).
narrative_ontology:measurement_basis(marr_su_t80, observed).
narrative_ontology:measurement(marr_su_t100, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 100, 0.61).
narrative_ontology:measurement_basis(marr_su_t100, observed).
narrative_ontology:measurement(marr_su_t120, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 120, 0.64).
narrative_ontology:measurement_basis(marr_su_t120, observed).
narrative_ontology:measurement(marr_su_t133, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 133, 0.49).
narrative_ontology:measurement_basis(marr_su_t133, observed).
narrative_ontology:measurement(marr_su_t153, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 153, 0.44).
narrative_ontology:measurement_basis(marr_su_t153, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'marriage law in India' fails the epsilon-invariance test: measuring authority-by-community yields a different epsilon than measuring authority-by-statute, and each measurement implies a different victim set. Per the decomposition rule the concept splits into five readings of one kernel, each a separate constraint story with its own epsilon, beneficiaries, victims, and classification. This file authors the christian_canonical_reading; the four sibling files author the others; these edges record family linkage, not a claim that one label measures one thing. The upstream/downstream asymmetry runs from the older communal readings toward the secular_civil_reading, whose legitimacy conditions are shaped by the communal readings' continued operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
