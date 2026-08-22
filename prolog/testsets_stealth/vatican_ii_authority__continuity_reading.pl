% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II Hermeneutic of Continuity (Continuity Reading of the Conciliar Authority Kernel)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   The arrangement under authorship is the post-conciliar doctrinal
 *   settlement of the Catholic Church as governed by the hermeneutic of
 *   continuity: the rule, official since the Council's close and restated
 *   pontificate by pontificate, that the sixteen documents of Vatican II are
 *   organic development of an unchanging deposit of faith, that
 *   post-conciliar reforms are legitimate when faithful to the texts, and
 *   that apparent ambiguities are resolvable through the tradition's own
 *   interpretive principles. The settlement is administered by the
 *   hierarchical magisterium alone, legitimates the reform program and its
 *   implementers, extends recognition outward to other communions, and
 *   maintains itself through a doctrinal-enforcement apparatus whose
 *   intensity has oscillated with successive pontificates' policy. This story
 *   authors that arrangement from the continuity reading's own seat:
 *   epsilon's referent is the standing arrangement (the settlement as it
 *   operates), the values are reading-indexed, and the claim and the metrics
 *   are independent authored facts — the reading claims pure coordination
 *   with no victims while honestly conceding real enforcement (0.50), real
 *   resistance (0.60), and modest nonzero extraction (0.18). The engine
 *   computes per-seat classifications from the structural data; divergence
 *   between the reading's rope claim and a computed extraction at the
 *   enforcement-object seat is the measurement this story exists to take.
 *
 * KEY AGENTS:
 *   - hierarchical_magisterium: agenda-setter (institutional / identity_locked) — administers the hermeneutic of continuity, adjudicates which reforms are faithful, disciplines unfaithful readings; collects the settlement's adjudicative proceeds
 *   - progressive_reformers_claiming_continuity: primary beneficiary (organized / constrained) — theologians, liturgists, and pastoral implementers whose post-conciliar work is legitimized as deposit rather than innovation
 *   - traditionalist_communities: cost-bearer / enforcement object (moderate / identity_locked) — communities maintaining pre-conciliar liturgical forms under irregular status and restricted liturgical access
 *   - ordinary_laity: beneficiary, secondary payer (powerless / identity_locked) — receive the reforms as intended beneficiaries; absorbed the implementation disruption of the 1960s-70s
 *   - ecumenical_partners: beneficiary (organized / mobile) — other Christian communions and non-Christian religions receiving recognition and dialogue
 *   - sedevacantist_communities: excluded (powerless / identity_locked) — hold the conciliar-era papal line itself invalid; stand outside the conversation the settlement conducts
 *   - doctrinal_historians: analytical observer (analytical / analytical) — document the conciliar process and the hermeneutical debate; collect no proceeds and bear no enforcement costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.18).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.5).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II Hermeneutic of Continuity (Continuity Reading of the Conciliar Authority Kernel)").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea').
narrative_ontology:cs_kernel_codification('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', fixed_text).
narrative_ontology:cs_authority_grounding('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', lineage).
narrative_ontology:cs_interpretation_layer_present('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea').
narrative_ontology:cs_reading_relation('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', foundational, conciliar_teaching_continuous_with_deposit).
narrative_ontology:cs_axiom_status(conciliar_teaching_continuous_with_deposit, holdable).
narrative_ontology:cs_axiom_grounding('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', conciliar_teaching_continuous_with_deposit, theological).
narrative_ontology:cs_axiom('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', foundational, conciliar_ambiguities_resolvable_by_tradition).
narrative_ontology:cs_axiom_status(conciliar_ambiguities_resolvable_by_tradition, holdable).
narrative_ontology:cs_axiom_grounding('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', conciliar_ambiguities_resolvable_by_tradition, theological).
narrative_ontology:cs_reference_frame('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', organic_development_of_immutable_deposit).
narrative_ontology:cs_drift_state('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', contemporary_post_conciliar_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('532d9e01-a2ed-4c19-b7e6-ef0307e2d2ea', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, hierarchical_magisterium).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, ordinary_laity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, ecumenical_partners).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, ordinary_laity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, organic_doctrinal_development_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, hermeneutic_of_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, immutable_deposit_of_faith).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The pope and college of bishops in their teaching office. They administer the settlement: issue authoritative interpretations of the sixteen documents, adjudicate which reforms are faithful to the texts, and discipline readings they judge unfaithful through the doctrinal congregation and canonical instruments. The adjudication function is theirs alone; no external body reviews their continuity judgments. The settlement's proceeds — the interpretive monopoly and the authority to declare what is faithful — accrue to this seat. Their horizon is the Church's own, measured in centuries, and their identity is fused with the office: abandoning the adjudicative role would dissolve the office itself.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, hierarchical_magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, hierarchical_magisterium, beneficiary).

% Theologians, liturgists, catechists, and pastoral implementers whose post-conciliar work derives its legitimacy from the continuity settlement: because the Council is continuous with tradition, their reforms are expressions of the deposit rather than innovations requiring separate justification. Their careers, faculties, and institutional standing sit inside the arrangement; exit would forfeit the ecclesial audience their work addresses, and their standing depends on remaining within the adjudicated bounds of faithfulness.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity, beneficiary,
    organized, biographical, constrained, global).

% Communities maintaining pre-conciliar liturgical forms and doctrinal emphasis — the Society of St Pius X and affiliated institutes. They hold irregular canonical status, their confessions and marriages are recognized only provisionally, and access to the older liturgy has been alternately widened and restricted by magisterial decree. The settlement's disciplinary costs fall on them specifically and track their liturgical allegiance. Their exit is bounded on both sides: regularizing on the arrangement's terms requires accepting the conciliar settlement they contest, while abandoning their liturgical and doctrinal allegiance would dissolve the community identity that constitutes them.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_communities, payer,
    moderate, generational, identity_locked, global).

% Receive the settlement's outputs — vernacular liturgy, ecumenical opening, catechetical reform — as its intended beneficiaries. They also absorbed the implementation disruption of the 1960s-70s: rapid liturgical change, catechetical discontinuity, and parish-level upheaval whose costs fell on them without their consent or adjudicated voice. Exit is bounded by baptismal identity and parish structure: leaving the communion is possible but carries the cost of religious displacement, and internal voice is mediated through structures they do not control.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, ordinary_laity, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, ordinary_laity, payer).

% Other Christian communions and non-Christian religions that receive recognition and structured dialogue from the settlement (the decree on ecumenism, the declaration on non-Christian religions). They sit outside the Catholic governance structure entirely; the settlement's benefits reach them without its internal discipline applying to them, and they can disengage without canonical consequence.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, ecumenical_partners, beneficiary,
    organized, biographical, mobile, global).

% Small communities holding the conciliar-era papal line itself invalid. The settlement's conversation does not admit their premise — its validity assumption pre-empts their objection rather than adjudicating it — so they stand outside the hermeneutical process altogether, with no canonical channel through which their position could be heard. Re-entering the conversation would require abandoning the premise that constitutes them.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, sedevacantist_communities, excluded,
    powerless, generational, identity_locked, global).

% Academic historians and theologians who document the conciliar process and the post-conciliar hermeneutical debate. They collect no proceeds from the settlement and bear none of its enforcement costs; their seat is analytical, though their publication venues and ecclesiastical standing can be affected by the arrangement's approval machinery, which gives the observer seat a dependent edge.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, doctrinal_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, hierarchical_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single authoritative interpretation of the sixteen conciliar documents so that the pre- and post-conciliar magisterium can be read as one deposit; adjudicates which post-conciliar reforms are faithful to the texts; provides a global, multigenerational communion with a common rule for binding doctrinal questions.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy toward the hierarchical magisterium, which alone adjudicates continuity, and toward post-conciliar reforms and their implementers, whose work the settlement legitimizes; moves disciplinary costs (canonical irregularity, liturgical restriction, provisional sacramental recognition) onto communities that maintain pre-conciliar forms; moves recognition and dialogue outward to other communions and religions.
% ABSENT_VOICES: Communities holding the conciliar documents themselves invalid stand outside the conversation entirely — the settlement's validity premise pre-empts their objection rather than adjudicating it, and no canonical channel exists through which it could be heard. Within the conversation, lay voices from the implementation generation who absorbed the liturgical disruption rarely enter the hermeneutical process; the conversation is conducted almost entirely among magisterial and theological professionals.
% DISAPPEARANCE_RATIONALE: Without an authoritative continuity hermeneutic, the question of how the conciliar documents bind would reopen across the communion: the reform program's legitimacy would become a case-by-case contest rather than a settled expression of the deposit, the communities maintaining pre-conciliar forms would lose the canonical frame that defines their irregularity (discipline presupposes the norm it enforces), and the magisterium's adjudicative monopoly would need reconstitution from first principles. The Church would persist, but its doctrinal self-governance would reorganize around the reopened question — the settlement's disappearance rearranges the arrangements of every named seat.
% FOUNDING_PROBLEM: After a council that reaffirmed much of the prior magisterium while introducing genuine terminological and doctrinal novelties (collegiality, religious liberty, the liturgical reform, the decree on non-Christian religions), the Church required a rule for reading the sixteen documents together with the preceding tradition — such that the Council could bind the whole communion without appearing to contradict what it had always taught.
% FOUNDING_PROBLEM_CORROBORATION: The 1965 minority Fathers' dissenting annotations and post-conciliar reservations attest that the interpretive difficulty was real and contested from the council's own floor; the critical historiographical school behind the multi-volume History of Vatican II — outside the benefiting parties and often hostile to the continuity hermeneutic — corroborates that the documents contain genuine tensions requiring adjudication; and the disciplined communities' own canonical testimony corroborates that the interpretation question remains live. No party to the dispute claims the founding problem was fictive.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored from the continuity reading's seat over the standing arrangement. Extractiveness 0.18: the reading holds the reforms are cost-free development and concedes only the interpretive discipline itself as a real cost — nonzero because the disciplinary costs are real and acknowledged even where their legitimacy is disputed. Suppression 0.50: the settlement's persistence depends on an active enforcement apparatus (doctrinal congregation interventions, canonical irregularity, liturgical restriction); the reading endorses this apparatus as legitimate governance rather than coercion, but the metric records the machinery's actual force, not its valuation — and the machinery currently sits at its post-2007 high. Theater 0.12: the reforms are functionally real — the vernacular liturgy happens, the dialogues occur, the collegial structures meet; a small performative residue accumulates where conciliar vocabulary is recited without its content. Accessibility_collapse 0.55: once the hermeneutic is granted, the rival hermeneutic collapses as incoherent within the framework, but it persists as a live held position, so collapse is partial — rope-typical. Resistance 0.60: organized, durable, and bilateral (traditionalist resistance to the settlement's discipline; progressive resistance to its limits). The temporal series run on one shared grid (T=0..60 mapping 1965..2025) so every metric is authored at every examined point. The suppression series oscillates rather than drifts monotonically: enforcement built through the 1970s-80s (peaking after the 1988 episcopal ordinations and excommunications), held high through the 1990s, eased during the 2007-2015 détente (universal permission for the older liturgy, remission of the excommunications), and re-intensified after the 2021 liturgical restrictions. The cycle is driven by successive pontificates' enforcement policy — an external governance driver, not intermittent reinforcement feeding on the oscillation; the base_properties scalars are measured at interval end (T=60, the re-tightened phase).
 *
 * PERSPECTIVAL GAP:
 *   The payer-side and beneficiary-side seats compute differently from the same structural data. From the magisterium's seat the arrangement is the Church's ordinary self-governance: the adjudicative monopoly is the teaching office functioning, discipline is fidelity, and the settlement's proceeds are indistinguishable from its purpose. From the progressive reformers' seat it is legitimation: their post-conciliar work counts as deposit rather than innovation. From the traditionalist communities' seat the same machinery operates as exclusion from the tradition they hold themselves to be preserving — irregular status, restricted liturgy, and provisional sacramental recognition are costs imposed for the allegiance, not for any regulatory aim they share. The laity's seat is near-symmetric: intended beneficiaries who also absorbed the implementation disruption. The engine computes these divergences from power, exit, and the directionality data; this story's claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive low d for the progressive reformers and, as agenda-setter-beneficiary, for the magisterium; the laity and ecumenical partners derive low d as declared beneficiaries. The traditionalist communities are this story's directionality correction: the derivation chain reads the beneficiaries/victims arrays, and because this reading declares no victims, their enforcement-object status is invisible to the structural derivation — they would derive as unaligned moderates near the symmetric point. The override (moderate power atom to d=0.65; the traditionalist communities are the story's only moderate-power seat, so the atom-level override binds to them alone) records the actual relationship: the settlement's disciplinary machinery operates on them specifically, its costs tracking their liturgical allegiance. The override does not re-author the reading's no-victim claim; whether d=0.65 at their seat computes as effective extraction is the engine's measurement, and a computed extraction there against the reading's rope claim is precisely the divergence this story exists to take.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a rule for reading the conciliar documents together with the preceding tradition — is live: every new doctrinal or liturgical question reactivates it, and the arrangement's enforcement activity tracks live disputes rather than a dead mandate. No mandatrophy is declared. The mismatch consumer reads founding_problem_status (live) against disappearance_verdict (world_rearranges): aligned, no capture/zombie flag. The watch-item is the suppression series: enforcement capacity has re-intensified after the 2007-2015 détente; if the founding problem were ever authoritatively declared resolved while the enforcement machinery persisted at current levels, the dead-mandate-plus-world_rearranges mismatch would fire and the arrangement would warrant re-examination as maintained by inertia. The rope claim also does boundary work in both directions: it keeps the rupture reading's high-extraction characterization from being imported into this story's classification, and the re-intensifying enforcement series keeps the rope claim from hardening into a no-enforcement complacency the record contradicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading — the continuity_reading — of the contested kernel vatican_ii_authority; what would change structurally if a sibling reading were adopted instead?',
    'Framework-level commitment, not resolvable by intra-framework data: the rupture_reading would re-author epsilon over the same referent as high (the arrangement as break, extracting from the tradition it claims to transmit), and the composite_overdetermination_reading would refuse a single epsilon altogether (no unified referent). The classification corpus takes the per-reading divergence over the shared referent as its measurement.',
    'Adopting a sibling reading changes the victim set, the epsilon value, and the computed type at every seat; this story''s rope claim and epsilon 0.18 are valid only within the continuity reading''s framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of the vatican_ii_authority kernel; sibling readings instantiate different constraints over the same referent.').

omega_variable(
    discipline_or_victimhood_boundary,
    'The reading declares no victims (reforms are cost-free development), yet the enforcement record shows costs — irregular canonical status, restricted liturgical access, provisional sacramental recognition — falling specifically on communities that maintain pre-conciliar forms. Are these bearers victims of the arrangement, or members under legitimate discipline?',
    'Compare treatment of structurally similar communities differing primarily in liturgical allegiance: if costs track allegiance rather than any regulatory aim the reading itself endorses (order, unity, doctrine), the cost-bearing is extraction with a victim class; if costs remit when communities regularize on the terms actually offered, they are discipline.',
    'If a victim class exists, the payer seat computes as extracted and the constraint''s profile shifts from rope toward tangled_rope at that seat; the reading''s no-victim structural claim fails and the beneficiaries'' coordination story becomes partial cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discipline_or_victimhood_boundary, conceptual, 'Whether the arrangement''s enforcement costs constitute victimization or legitimate discipline.').

omega_variable(
    continuity_discovered_or_constructed,
    'Is the continuity of the conciliar documents with the prior deposit a discovered structural fact that the hermeneutic tracks, or a constructed interpretive settlement that benefits identifiable agents (the magisterium''s adjudicative monopoly, the reformers'' legitimacy)?',
    'Test the hermeneutic''s necessity: if the documents'' continuity is evident on plain reading, the interpretive machinery is superfluous and its beneficiaries reveal construction; if the texts are genuinely ambiguous, examine whether the traditional hermeneutic is one admissible resolution among several and what distinguishes the chosen one from its rivals.',
    'If constructed-with-beneficiaries, the settlement''s coordination claim is partially cover and extraction analysis applies to the interpretive monopoly; if discovered, the arrangement is coordination tracking a real structure and the rope claim holds at every seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_discovered_or_constructed, conceptual, 'Whether doctrinal continuity is a discovered fact or a constructed settlement.').

omega_variable(
    traditionalist_exit_lock_mechanism,
    'Is the traditionalist communities'' identity_locked exit structural (canonical irregularity forecloses regular status while the liturgical allegiance persists) or internalized (communal identity fused with the contested fidelity, making regularization on the arrangement''s terms unthinkable)?',
    'Observe regularization episodes: the 2009 remission of excommunications, the personal-ordinariate offers, and the 2022-2024 Society negotiations — communities offered regular terms reveal whether the lock is the canonical door or the identity behind it.',
    'If structural, widening regular paths would shrink the enforcement-object class and the arrangement''s suppression requirement would fall; if internalized, enforcement costs persist regardless of accommodation and the suppression is carried by the agents themselves after every door opens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_exit_lock_mechanism, empirical, 'Structural vs internalized mechanism of the traditionalist exit lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__continuity_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__continuity_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__continuity_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__continuity_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__continuity_reading, theater_ratio, 50, 0.11).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__continuity_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__continuity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__continuity_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__continuity_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__continuity_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__continuity_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__continuity_reading, base_extractiveness, 50, 0.17).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__continuity_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__continuity_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__continuity_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__continuity_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__continuity_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__continuity_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__continuity_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__continuity_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The kernel vatican_ii_authority decomposes into three readings — continuity (this file), rupture, and composite_overdetermination — because the natural-language label 'the meaning of Vatican II' covers structurally distinct claims whose epsilon values differ over the same referent: this reading authors 0.18, the rupture reading authors high extraction over the identical standing arrangement, and the composite reading refuses a unified referent entirely. Each is a separate constraint story with its own beneficiaries, metrics, and classification; this file links both siblings via affects_constraints so contamination and drift propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__continuity_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
