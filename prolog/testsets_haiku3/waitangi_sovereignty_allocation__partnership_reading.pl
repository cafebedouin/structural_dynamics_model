% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi Partnership Reading: Consultation and Crown Moderation
 *   domain: constitutional/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) was signed in two materially different
 *   versions: the English text ceded sovereignty to the Crown; the Māori text
 *   guaranteed tino rangatiratanga (chiefly authority) to iwi. For 130 years
 *   the Crown governed as if it held unilateral sovereignty. From the 1970s
 *   onward, the Waitangi Tribunal (established 1975) and Court of Appeal
 *   developed a 'partnership' reading: the Treaty established an ongoing
 *   Crown-Māori partnership requiring good faith consultation, active
 *   protection of Māori interests, and Crown-funded settlement of historical
 *   grievances. This constraint story instantiates that partnership
 *   reading—not the Crown sovereignty reading (which treats the English text
 *   as dispositive and Parliament as supreme) nor the rangatiratanga reading
 *   (which treats the Māori text as retaining tribal authority and Crown
 *   power as limited to settlements with iwi). The partnership reading
 *   occupies the middle institutional ground: it moderates Crown power
 *   through consultation duties and principles doctrine while preserving
 *   parliamentary sovereignty and finite settlement remedies. The claim and
 *   metrics are deliberately asymmetric: this reading is claimed as
 *   tangled_rope (genuine coordination of Crown-Māori relations plus
 *   asymmetric extraction via constrained remedies), and the metrics reflect
 *   moderate extractiveness that has stabilized, rising theater as
 *   consultation becomes more performative, and persistent suppression via
 *   parliamentary override capacity and powerless-agent exclusion.
 *
 * KEY AGENTS:
 *   - Crown executive: Sets and administers partnership through Cabinet, controls settlement scope and funding, maintains parliamentary override capacity
 *   - Iwi treaty claimants (organized): Benefit from consultation rights and settlement remedies; constrained by finite settlements and advisory consultation status
 *   - Parliament: Retains formal sovereignty; can override partnership principles by statute
 *   - Māori individuals outside settlement: Identity-locked targets; bear historical costs without access to remedies or consultation voice
 *   - Rangatiratanga advocates: Excluded from official machinery; argue partnership undersells tino rangatiratanga retained in Māori text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.58).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.62).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Reading: Consultation and Crown Moderation").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, '1384ee15-dca1-4a71-8935-c086f432bc42').
narrative_ontology:cs_kernel_codification('1384ee15-dca1-4a71-8935-c086f432bc42', fixed_text).
narrative_ontology:cs_authority_grounding('1384ee15-dca1-4a71-8935-c086f432bc42', extraction).
narrative_ontology:cs_interpretation_layer_present('1384ee15-dca1-4a71-8935-c086f432bc42').
narrative_ontology:cs_reading_relation('1384ee15-dca1-4a71-8935-c086f432bc42', waitangi_sovereignty_allocation__crown_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('1384ee15-dca1-4a71-8935-c086f432bc42', waitangi_sovereignty_allocation__rangatiratanga_reading, coexists_with).
narrative_ontology:cs_axiom('1384ee15-dca1-4a71-8935-c086f432bc42', foundational, treaty_establishes_ongoing_partnership).
narrative_ontology:cs_axiom_status(treaty_establishes_ongoing_partnership, holdable).
narrative_ontology:cs_axiom_grounding('1384ee15-dca1-4a71-8935-c086f432bc42', treaty_establishes_ongoing_partnership, conventional).
narrative_ontology:cs_axiom('1384ee15-dca1-4a71-8935-c086f432bc42', foundational, consultation_constrains_crown_unilateralism).
narrative_ontology:cs_axiom_status(consultation_constrains_crown_unilateralism, holdable).
narrative_ontology:cs_axiom_grounding('1384ee15-dca1-4a71-8935-c086f432bc42', consultation_constrains_crown_unilateralism, deontological).
narrative_ontology:cs_axiom('1384ee15-dca1-4a71-8935-c086f432bc42', secondary, parliament_retains_ultimate_sovereignty).
narrative_ontology:cs_axiom_status(parliament_retains_ultimate_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1384ee15-dca1-4a71-8935-c086f432bc42', parliament_retains_ultimate_sovereignty, empirically_contingent).
narrative_ontology:cs_reference_frame('1384ee15-dca1-4a71-8935-c086f432bc42', partnership_framework_established_1975).
narrative_ontology:cs_drift_state('1384ee15-dca1-4a71-8935-c086f432bc42', contemporary_2020_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1384ee15-dca1-4a71-8935-c086f432bc42', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_collective_interests).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, iwi_treaty_claimants).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_individuals_outside_settlement).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, dispossessed_land_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, iwi_treaty_claimants).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, non_maori_settled_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the partnership framework through Cabinet decisions, ministerial appointments to settlement authorities, and judicial appointments. Declares commitment to consultation and Treaty principles while maintaining parliamentary sovereignty and the ability to override Māori interests where political priority diverges. Controls settlement funding, remedies scope, and the timing/pace of redress mechanisms.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Organized through formal iwi leadership structures and Treaty settlement processes. Benefit from consultation requirements that delay or constrain some Crown actions, from settlement redress that restores some assets and authority, and from the principles doctrine that frames certain Crown duties as mandatory. Also bear costs: settlement agreements lock in finite redress amounts, consultation outcomes are advisory (Crown still decides), and remaining power asymmetries mean Crown-designed processes frame what counts as adequate remedy.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, iwi_treaty_claimants, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, iwi_treaty_claimants, payer).

% The abstract entity whose interests the partnership framework claims to protect: collective Māori cultural continuity, land stewardship, resource rights, and self-determination. This is not an actor but a normative referent the partnership reading uses to adjudicate Crown duties.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_collective_interests, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(waitangi_sovereignty_allocation__partnership_reading, maori_collective_interests).

% Māori people whose iwi either cannot organize effectively, did not qualify under settlement criteria, or whose individual grievances fall outside formal settlement process scope. They carry the identity, cultural stakes, and historical injury but lack access to the remedies and consultation mechanisms the partnership framework offers to formally organized claimants. Bear the costs of the constraint (unresolved historical wrongs) without the beneficiary mechanisms.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_individuals_outside_settlement, payer,
    powerless, biographical, identity_locked, national).

% Māori and whānau who lost specific lands but fall outside iwi settlement scope or whose claims pre-date the Crown's acknowledgment. They cannot exit the constraint (the land remains alienated, the dispossession persists), cannot renegotiate the settlement terms (those are locked agreements their iwi leadership negotiated without them), and lack voice in the consultation machinery because the machinery operates at the iwi/Crown level, not the individual claimant level.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, dispossessed_land_beneficiaries, payer,
    powerless, biographical, trapped, national).

% Reviews Crown actions and settlement agreement applications for compliance with the principles doctrine. Can declare Crown action a breach of the duty to consult, can interpret settlement language, but cannot override parliamentary statute or force Crown compliance if Parliament explicitly legislates contrary to principles. Acts as legitimacy-checker of the partnership framework within the bounds Parliament allows.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_judiciary, observer,
    institutional, generational, analytical, national).

% Holds formal parliamentary sovereignty: can pass legislation that overrides the principles doctrine, can redefine settlement scope, can change consultation requirements by statute. The partnership framework's authority ultimately rests on Parliament's willingness to respect it; Parliament retains the power to unilaterally abrogate or redefine the constraints.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% European settlers and their legal successors who hold title derived from Crown alienation of Māori lands. In settlement scenarios, may be required to surrender title to Crown-purchased land that is then returned to iwi, or face restrictions on Crown land use that affect their interests (conservation covenants, co-management agreements). Carry costs through asset transfers or use restrictions; can exit by selling encumbered land or relocating, but face costs in asset value or market disruption.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, non_maori_settled_interests, payer,
    powerful, biographical, mobile, national).

% Radical iwi sovereignty movements and constitutional scholars who read the Māori text as retaining tino rangatiratanga (full tribal authority) and argue the partnership reading undersells Māori sovereignty. They are excluded from the official consultation machinery because the partnership framework treats them as having a different constitutional vision than the negotiated settlement apparatus, even though their constituency includes many settlement participants.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_executive).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a procedural framework for ongoing Crown-Māori engagement on matters affecting Māori interests: requires Crown consultation before major policy decisions, obligates good faith negotiation of historical grievances through formal settlement mechanisms, and mandates that Crown actions respect principles of partnership derived from the Treaty. Solves the problem of how a post-colonial state can maintain legitimacy with an indigenous population while preserving Westminster parliamentary structure.
% TRANSFER_FUNCTION: Moves remedial resources (land, monetary settlement, co-management rights) from the Crown to organized iwi claimants in exchange for closure of specific historical grievances. Transfers authority over specific domains (fisheries, cultural property, resource-consent processes) from pure Crown control to shared or iwi-primary governance. Transfers intangible legitimacy: the Crown claims to govern with Māori consent and Māori interests as a constitutive principle, gaining political legitimacy; iwi claim Crown recognition of Treaty authority and contemporary self-determination, gaining institutional standing.
% ABSENT_VOICES: Rangatira who reject the partnership reading as inadequate to tino rangatiratanga; Māori individuals and whānau outside formal iwi structures who are excluded from settlement negotiations; dispossessed land claimants whose grievances pre-date Crown acknowledgment or fall outside settlement scope. These voices argue the partnership framework locks in a subordinate position by accepting Crown paramountcy and finite remedies in exchange for consultation rights that Crown can still override via parliamentary statute.
% DISAPPEARANCE_RATIONALE: If the partnership framework and its consultation requirements vanished, the Crown would have unfettered parliamentary power to legislate on Māori interests without procedural constraint, settlement authorities would dissolve, and co-management arrangements would revert to Crown unilateral control. The political legitimacy the Crown derives from Treaty partnership would evaporate, likely triggering sustained non-cooperation from iwi on resource consents, governance participation, and law enforcement. The framework structures how decisions get made; its absence removes the procedural gates that moderate Crown unilateralism.
% FOUNDING_PROBLEM: The 1840 Treaty of Waitangi was signed in two languages with materially different terms: the English Article I ceded sovereignty to the Crown; the Māori Article II guaranteed tino rangatiratanga (full chiefly authority) to iwi. By the 1970s, the Crown had unilaterally alienated most Māori land and sidelined Māori from governance. The partnership reading emerged as a political solution: acknowledge the Treaty's continued normative force, commit to consultation and redress processes, and frame Crown-Māori relations as a binding partnership with Crown acting in good faith.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and iwi settlement authorities affirm the founding problem persists (historical injustices remain unresolved, Crown consultation is incomplete). Rangatira advocating tino rangatiratanga reject that the partnership framework adequately addresses it, arguing consultation without substantive authority-sharing is symbolic without transformative effect. International indigenous rights bodies (UN Permanent Forum on Indigenous Issues, ILO Convention 169 monitors) attest the partnership framework provides meaningful procedural rights but notes persistent power imbalances and remaining dispossession. Academic legal scholars outside benefiting parties (both critical indigenist and orthodox constitutional scholars) dispute whether the partnership genuinely moderates Crown power or merely legitimates ongoing Crown paramountcy through consultation theater.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures at 0.58 (moderate-high) and is plateau-flat from year 15 onward, reflecting: (1) genuine coordination benefit (Crown gains legitimacy, iwi gain procedural standing), but (2) asymmetric outcome (settlement amounts are Crown-determined, consultation is advisory, Parliament retains override). The plateau indicates the framework has reached equilibrium—further settlements are negotiated but don't change the structural imbalance. Theater rises steeply from year 0 to 15 (from 0.25 to 0.38) then plateaus at 0.41, suggesting: early period was heavy on substantive settlement work; by year 15 consultation became more procedural and performance-heavy (co-management boards meet regularly but executive decisions shift minimally; consultation reports are published but recommendations often ignored). Suppression is steady (0.50→0.62 by year 15, then flat), reflecting: Crown maintains the capacity to override via parliamentary statute (structural suppression); iwi outside settlement structures are identity-locked and have no exit (suppression of alternatives for individuals). Accessibility collapse is low (0.48) because: alternatives exist (litigation, non-cooperation, migration, international advocacy), but each carries high cost for powerless agents. Resistance is high (0.71) because: rangatiratanga movements, individual Māori activists, and excluded iwi continuously challenge the partnership as insufficient. The measurement grid is shared—every metric authores a value at every examined time point, preventing grid misalignment that could date type transitions early.
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests as tangled_rope at the institutional level (Crown + organized iwi negotiate settlements) and piton at the individual level (Māori outside settlement structures face performance without substance). The ambiguity is structural, not a measurement error. Organized iwi settlement bodies participate in governance and receive Crown recognition of grievance legitimacy. Individuals whose iwi cannot organize, or whose claims pre-date Crown acknowledgment, are stuck: the constraint carries the cultural stakes and historical weight (identity-locked suppression) while the remedial machinery operates above them.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown executive: d ≈ 0.10–0.15 (beneficiary end). Controls the constraint, sets settlement terms, derives legitimacy from partnership framing, maintains institutional power. Parliamentary override capacity and control of consultation scope mean exit is arbitrage (Crown could unilaterally abrogate but chooses partnership for legitimacy gain). Iwi treaty claimants: d ≈ 0.50–0.60 (moderate target). Gain procedural standing and settlement remedies, but settlements are negotiated under Crown power asymmetry, consultation is advisory, and negotiated closure prevents reopening. Exit is constrained (cannot renegotiate closed settlements, cannot exit identity as iwi). Parliament: d ≈ 0.05 (pure beneficiary from the constraint's legitimacy). Retains formal sovereignty, uses partnership as a source of democratic consent. Māori individuals outside settlement: d ≈ 0.85–0.95 (full target end). Identity-locked (Māori identity makes them stakes-holders), trapped (cannot exit the dispossession, cannot renegotiate settlements iwi leadership made), excluded from consultation machinery (settlement agreements bind whole iwi). Dispossessed land beneficiaries: d ≈ 0.90–1.0 (full target end). Specific land losses remain permanent, settlement amounts are not negotiable, Crown acquisition of land for return is Crown-paced and Crown-funded. Directionality differs for non-Māori settled interests: d ≈ 0.65–0.75 (high target). Required to surrender land title or accept use restrictions; exit is possible (sell, relocate) but costly (asset value erosion, transaction costs). The override mechanism (parliamentary statute) keeps all non-beneficiary d values vulnerable to reclassification—Parliament could legislate settlement invalidation or consultation removal, making even organized-iwi exit technically mobile if Parliament shifted. This constitutional fragility is reflected in the high theater and suppression measurements.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint sits at the mandatrophy boundary. The founding problem (historical Crown dispossession + unilateral governance alienating Māori) was real. The partnership reading claims it remains 'live' (ongoing need for reconciliation, consultation, and justice). Yet the measurement data shows: (1) extractiveness plateaus at year 15, suggesting the remedial function has equilibrated rather than continuing to resolve wrongs, (2) theater rises as consultation becomes more procedural, suggesting legitimacy maintenance is increasingly performative rather than substantive, (3) suppression remains constant because parliamentary override capacity never diminishes (the Crown never surrenders the power to abrogate the constraint). Mandatrophy is signaled by the mismatch: founding_problem_status is 'contested' (Crown and iwi say 'live,' rangatiratanga advocates say 'dead—we settled into subordination'), and disappearance_verdict is 'world_rearranges' (the partnership structure does matter to governance), but the theater rise suggests that what matters is increasingly the legitimacy performance (partnership rhetoric) rather than substantive remedial outcome. The constraint has not yet crossed into full piton (it still delivers settlement resources and co-management rights), but the plateau in extractiveness and rise in theater are the leading edges of mandatrophy drift: if the constraint persists primarily to maintain Crown legitimacy while substantive remedies cease expanding, it has become partly zombie—it justifies (through partnership narrative) the Crown's fundamental power asymmetry rather than genuinely moderating it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consultation_substantive_vs_performative,
    'Does the Crown''s consultation obligation operate as substantive co-decision-making (consultation shapes outcomes) or performative legitimacy (consultation occurs but Crown decides unilaterally)?',
    'Analysis of post-consultation Crown decisions: if Crown acceptance rate of iwi recommendations is near or below baseline random choice, consultation is performative; if acceptance rate exceeds 60%, consultation is substantive.',
    'If performative, extractiveness should reclassify upward (snare-proximity); the partnership would be revealed as cover for Crown unilateralism. If substantive, current tangled_rope classification stands; partnership genuinely moderates power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consultation_substantive_vs_performative, empirical, 'Whether consultation shapes Crown outcomes or legitimates predetermined decisions.').

omega_variable(
    settlement_finality_vs_reopening,
    'Do settlement agreements represent final Crown acknowledgment of historical wrongs and permanent remedy, or do they function as capital controls on Māori claims (grievances closed, cannot renegotiate even if circumstances change)?',
    'Review of settlement deed language, case law on res judicata, and Crown behavior when new evidence of historical wrongs emerges post-settlement. If Crown permits reopening on new evidence, agreements are genuine; if Crown enforces settlement closure as absolute bar, agreements are capital-control.',
    'If capital-control, extractiveness rises (snare-adjacent: Māori exchange unlimited future claims for finite present remedy); if genuine, extractiveness reflects accurate asymmetry (tangled_rope maintained). Theater_ratio also rises under capital-control interpretation (settlement becomes closure theater rather than remedial substance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settlement_finality_vs_reopening, empirical, 'Whether settlements resolve wrongs or close off future grievance redress.').

omega_variable(
    parliamentary_override_vs_partnership_binding,
    'Is the partnership reading binding on Parliament (Parliament cannot legislate contrary to principles without explicit repudiation), or is Parliament''s legislative supremacy absolute (Parliament can override partnership via statute without constitutional barrier)?',
    'Constitutional law doctrine and court rulings on implied repeal, statutory interpretation against breach of principles, and whether Parliament must expressly repudiate Treaty principles to legislate contrary to them. Test case: if Parliament passes legislation that conflicted with principles and courts enforce it without requiring explicit repudiation language, parliamentary override is absolute.',
    'If override is absolute, suppression never decreases (Parliament is the ceiling on all constraint-moderation attempts) and extractiveness is structurally asymmetric (the Crown can always escalate unilaterally). If partnership is binding, Parliament faces political costs for explicit override, and extractiveness can shift downward if parliamentary willingness to override erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_override_vs_partnership_binding, conceptual, 'Whether the partnership reading constrains Parliament or Parliament retains unilateral power to abrogate it.').

omega_variable(
    kernel_reading_identity,
    'Is the partnership reading internally coherent as a distinct reading, or does it collapse into either Crown sovereignty (if Parliament retains full override) or rangatiratanga (if consultation concedes real authority-sharing)?',
    'Structural analysis: does the partnership reading maintain a stable middle position—consultation that moderates but doesn''t control Crown, settlement that remedies but doesn''t renegotiate parliamentary scope, principles that constrain but don''t override—without either pole collapsing it? If court doctrine or parliamentary practice pushes consultation toward pure performance, the reading collapses toward Crown sovereignty; if settlement agreements begin to concede iwi co-legislating power, it approaches rangatiratanga.',
    'If the reading collapses, the partition of the kernel breaks down—two readings remain active (Crown sovereignty and rangatiratanga) and the partnership-reading constraint must be reclassified to one of them (likely Crown sovereignty if collapsing, triggering downstream network contamination and classification shift from tangled_rope to piton or snare). If the reading remains stable, its classification holds, but the kernel itself remains contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the partnership reading maintains its own stable identity or collapses into an adjacent reading.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Does the measured suppression (0.62) reflect structural barriers imposed by the Crown (parliamentary override capacity, settlement finality, consultation advisoriness), or internalized suppression—Māori acceptance of partnership terms as legitimate even when they constrain Māori agency?',
    'Ethnographic and interview-based evidence from iwi negotiators and claimants. If suppression persists even when Crown removes structural barriers (hypothetically: Parliament passes legislation binding consultation to majority-iwi preference), the suppression is internalized; if suppression collapses when barriers are removed, it is structural.',
    'If internalized, effective suppression exceeds the 0.62 metric (the target carries suppression into post-exit scenarios, limiting the constraint''s removal even if parliamentary override occurred). If structural, reclassifying post-barrier-removal would be appropriate. The distinction affects how seriously to weight theater_ratio: structural suppression uses theater to maintain barriers; internalized suppression uses theater to maintain psychological compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether measured suppression is structural coercion or internalized psychological compliance.').

omega_variable(
    rangatiratanga_reading_viability,
    'Is the rangatiratanga reading a coherent alternative vision of Crown-Māori relations (treating Māori governments as co-sovereign, not subordinate to Parliament), or is it a negotiating position from which organized iwi have retreated into partnership settlement?',
    'Historical analysis of rangatira positions from 1975 onward: did organized iwi leadership ever commit to rangatiratanga as the governing framework, or was it always a negotiating demand that settlements walked back to partnership? If iwi explicitly renounced rangatiratanga for partnership, the reading is residual (held by advocates but not institutional base). If iwi maintained rangatiratanga as preferred option, the reading is live (institutional base exists but settled into partnership under power constraints).',
    'If rangatiratanga is residual, the partnership reading''s stability is higher (main institutional reading has converged). If rangatiratanga is live but settled, the partnership reading is precarious (could reignite if political conditions shift); this affects interpretation of ''founding_problem_status: contested''—if rangatiratanga is still live at the institutional base, the partnership is more vulnerable to reclassification as a piton if settlements cease delivering substantive redress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rangatiratanga_reading_viability, empirical, 'Whether rangatiratanga is a live institutional alternative or a residual advocacy position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(wait_tr_t0, observed).
narrative_ontology:measurement(wait_tr_t5, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(wait_tr_t5, observed).
narrative_ontology:measurement(wait_tr_t10, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(wait_tr_t10, observed).
narrative_ontology:measurement(wait_tr_t15, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(wait_tr_t15, observed).
narrative_ontology:measurement(wait_tr_t20, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(wait_tr_t20, observed).
narrative_ontology:measurement(wait_tr_t25, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(wait_tr_t25, observed).
narrative_ontology:measurement(wait_tr_t30, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(wait_tr_t30, observed).
narrative_ontology:measurement(wait_tr_t35, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(wait_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(wait_be_t0, observed).
narrative_ontology:measurement(wait_be_t5, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(wait_be_t5, observed).
narrative_ontology:measurement(wait_be_t10, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(wait_be_t10, observed).
narrative_ontology:measurement(wait_be_t15, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(wait_be_t15, observed).
narrative_ontology:measurement(wait_be_t20, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(wait_be_t20, observed).
narrative_ontology:measurement(wait_be_t25, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(wait_be_t25, observed).
narrative_ontology:measurement(wait_be_t30, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(wait_be_t30, observed).
narrative_ontology:measurement(wait_be_t35, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement_basis(wait_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(wait_su_t0, observed).
narrative_ontology:measurement(wait_su_t5, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(wait_su_t5, observed).
narrative_ontology:measurement(wait_su_t10, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(wait_su_t10, observed).
narrative_ontology:measurement(wait_su_t15, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(wait_su_t15, observed).
narrative_ontology:measurement(wait_su_t20, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(wait_su_t20, observed).
narrative_ontology:measurement(wait_su_t25, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(wait_su_t25, observed).
narrative_ontology:measurement(wait_su_t30, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(wait_su_t30, observed).
narrative_ontology:measurement(wait_su_t35, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(wait_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__partnership_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, treaty_settlement_process__te_ture_whenua_land_claims).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, maori_fisheries_quota_regime).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel waitangi_sovereignty_allocation. The partnership reading mediates between the crown_sovereignty reading (treating the Treaty as historical settlement with no ongoing normative force) and the rangatiratanga reading (treating the Māori text as retaining tribal authority). All three are live institutional readings held by different constituencies. They are NOT alternative observables of one constraint—they are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different type classifications. The partnership reading's ε (0.58, moderate extraction under coordination framing) differs from crown_sovereignty (low ε, mountain-like) and rangatiratanga (high ε, snare-like). Each reading is authored as a separate constraint story linked via this network field. The decomposition reflects OQ-83 ruled methodology: kernel readings are distinct constraints, not measurement ambiguities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, analytical, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
