% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligation as Destructive Extraction Cycle
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   Post-Carolingian Western Europe, c. 900-1300, in regions where royal
 *   enforcement capacity had collapsed: kin groups answered homicide and
 *   injury through obligatory feud — declared enmity, retaliatory raiding,
 *   and negotiated wergild — under norms that made refusal to answer a
 *   kinsman's death a surrender of standing. This story authors the
 *   extraction_cycle_reading of that arrangement: the obligation operates as
 *   a destructive cycle draining kin economies of men, labor, and land, while
 *   royal authority, positioning itself as the only cure for the disorder,
 *   converts the feud's persistence into legitimacy and fiscal yield through
 *   peace-purchase, breach fines, wergild shares, and protection-justified
 *   taxation. The epsilon referent is the standing feud-obligation
 *   arrangement itself, assessed by this reading's own lights. This file is
 *   one member of a three-story constraint family decomposing the
 *   feud_obligation_kernel; its siblings are separate constraint files linked
 *   through the network edges below, and nothing is averaged across readings.
 *   KEY AGENTS (by structural relationship): - royal_authority: Primary
 *   beneficiary (institutional/arbitrage) — collects legitimacy and fiscal
 *   rents from feud operation - feuding_kin_group_members: Primary target and
 *   self-administrator (organized/identity_locked) — bears mortality and
 *   wealth drain while enforcing the obligation internally -
 *   kin_group_dependents: Secondary target (powerless/trapped) — funds
 *   wergild and absorbs ravage without standing - feud_settlement_mediators:
 *   Incidental beneficiary (moderate/mobile) — collects brokerage fees from
 *   every live feud - borderland_peasant_communities: Excluded voice
 *   (powerless/trapped) — bears passage damage with no seat in the framework
 *   - comparative_legal_historians: Analytical observer — sees the full
 *   circuit no participant observes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.72).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.83).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligation as Destructive Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '9bb8e518-3eee-4747-b54d-2556a37452b7').
narrative_ontology:cs_kernel_codification('9bb8e518-3eee-4747-b54d-2556a37452b7', distributed).
narrative_ontology:cs_authority_grounding('9bb8e518-3eee-4747-b54d-2556a37452b7', practice).
narrative_ontology:cs_interpretation_layer_present('9bb8e518-3eee-4747-b54d-2556a37452b7').
narrative_ontology:cs_reading_relation('9bb8e518-3eee-4747-b54d-2556a37452b7', feud_obligation_kernel__stateless_coordination_reading, influences).
narrative_ontology:cs_reading_relation('9bb8e518-3eee-4747-b54d-2556a37452b7', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('9bb8e518-3eee-4747-b54d-2556a37452b7', foundational, feud_net_depletes_productive_capacity).
narrative_ontology:cs_axiom_status(feud_net_depletes_productive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('9bb8e518-3eee-4747-b54d-2556a37452b7', feud_net_depletes_productive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('9bb8e518-3eee-4747-b54d-2556a37452b7', foundational, private_vendetta_blocks_territorial_consolidation).
narrative_ontology:cs_axiom_status(private_vendetta_blocks_territorial_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('9bb8e518-3eee-4747-b54d-2556a37452b7', private_vendetta_blocks_territorial_consolidation, empirically_contingent).
narrative_ontology:cs_reference_frame('9bb8e518-3eee-4747-b54d-2556a37452b7', enforcement_vacuum_self_help_order).
narrative_ontology:cs_drift_state('9bb8e518-3eee-4747-b54d-2556a37452b7', high_medieval_royal_justice_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9bb8e518-3eee-4747-b54d-2556a37452b7', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, feud_settlement_mediators).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feuding_kin_group_members).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, kin_group_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds fragmented sovereignty across a realm where kin groups wage private war. It prices participation in public order: sells protection, fines breach of the king's peace, takes shares of wergild, and levies taxation justified by the protection it offers against the disorder the feud sustains. Each feud it fails to prevent becomes both revenue and a fresh argument that only royal justice can secure the realm. Its exit is not from the arrangement but between postures — tolerating feud for fiscal yield or crushing it for jurisdictional gain — and it arbitrages the two as its administrative capacity grows.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).

% Free kinsmen bound to answer a killing against their line with retaliation or negotiated wergild. They enforce the obligation on one another through honor sanction: a man who lets a kinsman's death go unanswered loses standing, marriage prospects, and his lineage's capacity to deter future attack. They administer the obligation in assembly — declaring feud, setting terms, mustering raiding parties — while paying for it in dead brothers, burned harvests, guarded fields left unsown, and compensation debt. Renouncing the obligation would dissolve the kinship standing that constitutes who they are; exit is not a door they can walk through without ceasing to be what they are.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feuding_kin_group_members, payer,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, feuding_kin_group_members, agenda_setter).

% Household members, tenants, and herders attached to feuding lineages. They fund wergild from grain and livestock, lose working hands to guard duty and raiding service, and absorb reprisal raids aimed at lineage wealth. They receive whatever protection lineage deterrence affords but chose none of the obligations that generate the danger. Flight means abandoning land, kin, and subsistence together; staying means financing a quarrel conducted entirely above their station.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, kin_group_dependents, payer,
    powerless, biographical, trapped, local).

% Neutrals — elders, allied chieftains, oath-helpers — who broker truces and wergild settlements between hostile lineages. Every live feud generates demand for their services, paid in gifts, fees, and standing. They profit from the obligation's operation without bearing its violence, and their mobility between districts insulates them from the cycles they negotiate.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_settlement_mediators, beneficiary,
    moderate, biographical, mobile, local).

% Villages lying between feuding territories. Raiding parties cross their land, requisition stores, and burn what cannot be carried off. They hold no standing in feud councils, appear in no wergild tariff, and cannot bring claims in the kin-law framework, which prices only the lives of lineage members. They would object to a justice order that renders their losses invisible; they are not in the conversation because the conversation is constituted by kinship membership.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, borderland_peasant_communities, excluded,
    powerless, biographical, trapped, local).

% Reconstruct the feud's economics from charters, saga proceedings, wergild tariffs, and peace legislation across post-Carolingian Europe and stateless comparators such as saga-era Iceland, segmentary lineage societies, and customary-law highlands. They see the full circuit — kin expenditure, mediator capture, royal fiscal yield — that no participant seat observes whole.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In regions lacking centralized enforcement, the feud obligation supplies deterrence (an aggressor faces lineage-wide retaliation, not a single victim's weakness) and a compulsory path to settlement (wergild negotiation with assembly-brokered terms), coordinating kin-group solidarity and dispute resolution where no state adjudicates.
% TRANSFER_FUNCTION: Moves wealth (wergild, peace-purchase payments, breach fines), labor (guard duty, raiding service, unsown fields), and lives (killed men) out of productive circulation within kin economies; transfers legitimacy and fiscal receipts upward to royal authority, which prices and taxes the peace the feud destabilizes.
% ABSENT_VOICES: Unfree dependents, widows, and borderland peasant communities bear ravage, requisition, and labor loss but hold no standing in feud councils and no line in any wergild tariff; they would object to a justice order that compensates only lineage-member lives. They are absent because the framework's membership criterion is kinship itself.
% DISAPPEARANCE_RATIONALE: If the feud obligation vanished overnight, kin groups would need an immediate substitute for deterrence and compelled settlement — predation against unprotected lineages would spike until some authority absorbed the function. Royal authority would lose its primary legitimacy narrative and several fiscal hooks (breach fines, peace-purchase, protection taxation) until it consolidated direct jurisdiction; marriage alliance strategy, land markets, and settlement patterns, all priced against feud risk, would rearrange around whatever replaced it.
% FOUNDING_PROBLEM: The obligation crystallized where central enforcement capacity collapsed after Carololingian fragmentation: kin groups needed credible deterrence against predation and a mandatory settlement path so that killings did not go unanswered and disputes did not cascade unbounded.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legal-anthropological field studies of stateless feud societies (segmentary lineage pastoralists, saga-era Iceland, customary-law highland codes) attest the deterrence-and-settlement problem the obligation addressed; royal charters and chronicles independently document the enforcement vacuum of the ninth and tenth centuries. Royal authority attests the problem is solved only where its own courts now reach — a self-interested attestation — while feud revivals in under-governed margins (Scottish borders, Mediterranean islands under nominal state law) attest the problem lived on wherever court reach failed.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.72 (interval end) because the cycle consumes the kin economy's core inputs — fighting-age men, harvest labor diverted to guarding, livestock and land alienated to meet wergild — while the crown layers monetary take (fines, shares, peace-purchase) on top of the physical destruction. Suppression is authored at 0.83 as a raw structural property, unscaled by power or scope: exit is closed by kin liability, retaliation risk against deserters' kin, and honor sanction, not by anything the engine scales; only extractiveness is scaled downstream. Theater rises monotonically from 0.14 to 0.45 as the feud's substance migrates into courts and its residue formalizes into declaration rituals and nominal enmities — Goodhart drift of the obligation's original function. Accessibility_collapse is 0.55: composition payments, purchased peace, litigation, and emigration existed as alternatives, but the honor economy blunted their uptake, so alternatives persist without being usable at scale. Resistance is 0.60: truce movements, settlement drives, litigation, and emigration met the obligation continuously from both inside (wearied kinsmen) and outside (church and crown campaigns). The claimed type (tangled_rope) is authored from structure — a real deterrence-and-settlement coordination function, asymmetric extraction falling on the coordinated parties plus external capture by the crown, and active enforcement through kin sanction — independently of the metric values; the engine computes per-seat types from the structural data, and any divergence between claim and computation is the measurement the corpus exists to take. All three tracked series run on one shared nine-point grid (900-1300, half-century steps) so every metric is authored at every examined time point; the suppression series is included because the story specifically traces enforcement-capacity change (the build-out of royal criminal jurisdiction), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the royal seat, the feud is a yield-bearing disorder: every outbreak validates the monarchy's jurisdictional claim and feeds its fisc, so the arrangement presents as opportunity administered rather than burden borne. From the kinsman's seat, the same arrangement is an inescapable obligation that consumes his line's men and land and that he himself helps enforce in assembly — payer and administrator fused in one body. The mediator seat sees fee flow without violence; the dependent seat pays for a quarrel it never joined. The engine derives these divergent per-seat classifications from the declared roles, power atoms, and exit options; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map directly onto the structural relationships. royal_authority is declared beneficiary with arbitrage exit: it subsidizes on the arrangement's persistence and can reprice its posture at will, placing it near the full-beneficiary end (low d, damped or inverted effective extraction). feud_settlement_mediators are beneficiaries with mobile exit — low d, insulated skimmers. feuding_kin_group_members are declared victims with identity_locked exit: trapped targets sit nearest the full-target end, so their effective extraction is amplified above base. kin_group_dependents are victims with trapped exit — similarly amplified. The excluded borderland communities sit outside the arrangement's formal structure entirely; their absence is commentary-grade (R3) and drives no directionality or override. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options produces the correct d for every seated agent, so overriding would only obscure the derivation the corpus is testing. Scope is national for the crown's seat and local for the kin seats; the engine's scope modifier applies to the constraint, with the crown's wide-realm extraction facing harder verification than any single district feud.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Reading the feud as pure extraction erases the genuine coordination that made it rational in an enforcement vacuum — deterrence and compelled settlement were real functions no contemporaneous institution supplied, which is precisely the stateless sibling's insight preserved as this file's coordination half. Reading it as pure coordination erases the mortality, the wealth drain, and the crown's rent capture riding on the disorder. The hybrid holds both: coordination function real, extraction layered on it, enforcement active throughout. On obsolescence: the founding problem (security in an enforcement vacuum) is authored contested, not dead — royal justice resolved it inside court reach while feud revivals persisted at the margins, so the arrangement's mandate decayed unevenly rather than expiring. The mismatch consumer reads founding_problem_status x disappearance_verdict: contested x world_rearranges yields no zombie flag, correctly — the arrangement rearranged the world while its warrant was being disputed, and the rising theater series tracks the hollowing without asserting completion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the extraction_cycle_reading of the feud_obligation_kernel; would the sibling readings (stateless_coordination_reading, christianized_pacification_reading) assign a different epsilon, victim set, or type to the same standing arrangement?',
    'Compile and compare the sibling story files authored over the same referent; the divergence in their computed classifications locates the kernel contest structurally rather than rhetorically.',
    'If the stateless_coordination_reading computes low-extraction coordination where this file computes 0.72 extraction, the contest sits in the coordination-function premise, not in the victim data; classification of the kernel as a whole is undefined until the readings are compared side by side.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this file is one reading of a contested kernel; sibling readings are separate constraints, not averaged here.').

omega_variable(
    coordination_premise_location,
    'Is the feud''s deterrence-and-settlement function the constraint''s purpose, or the cover beneath a depletion cycle? This is the specific structural element on which the kernel''s readings disagree.',
    'Compare kin-group security and economic outcomes in feud-operating versus feud-suppressed regions holding royal enforcement capacity constant; if security outcomes hold or improve under suppression at equal capacity, the deterrence function was separable from the obligation and the extraction reading''s premise strengthens.',
    'Resolution toward cover flips the computed type toward pure extraction with the coordination story as justification; resolution toward purpose supports the hybrid reading with a larger share of measured extraction reclassified as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_premise_location, conceptual, 'Locates the inter-reading disagreement in the coordination-function premise: purpose versus cover.').

omega_variable(
    christianized_sibling_restructure,
    'How would instantiating the christianized_pacification_reading restructure this arrangement''s beneficiary and victim sets?',
    'Author the sibling file: under the divine-law reading, every vengeance-taker enters the transgressor set regardless of position in the cycle, and ecclesiastical institutions collecting penance, endowment, and jurisdiction enter the beneficiary set in place of royal fiscal capture.',
    'The victim set expands from cycle-participants to all feud actors, the beneficiary seat shifts from royal_authority to ecclesiastical authority, and every directionality value in this file would be invalid for that reading — the two files must not share metrics despite sharing a referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(christianized_sibling_restructure, conceptual, 'Structural delta a sibling reading would impose: relocated victims, relocated beneficiary, reshuffled directionalities.').

omega_variable(
    wergild_flow_asymmetry,
    'Did wergild and composition flows net-extract from weaker lineages to stronger ones, or did they roughly balance across the feud population?',
    'Ledger and charter analysis of wergild payments against receipts by lineage rank across the interval; asymmetry would show strong lineages converting feud claims into a transfer channel from weak neighbors.',
    'A strong-to-weak net flow adds an internal capturer seat (magnate lineages) alongside the crown, deepening measured extraction and pushing the computed type toward pure extraction; balanced flows keep the capture external to the feud itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wergild_flow_asymmetry, empirical, 'Whether an internal capturer seat exists inside the kin system, hidden behind formally reciprocal compensation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of exit structural (kin liability rules, retaliation risk against deserters'' relatives) or internalized (honor identity that persists after external barriers fall)?',
    'Compare feud persistence across regions with equal court access but different honor cultures; post-substitution revivals (Corsican vendetta under functioning French courts, border-country feud under statute) indicate internalized carry-through.',
    'If a large share is internalized, true suppression exceeds the structural measure and exit_option atoms remain sticky even after royal justice removes the external barriers; classification consequences concentrate in the identity-locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized split of the suppression holding feud participants in place.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 900, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t900, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 900, 0.14).
narrative_ontology:measurement(feud_tr_t950, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 950, 0.17).
narrative_ontology:measurement(feud_tr_t1000, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1000, 0.21).
narrative_ontology:measurement(feud_tr_t1050, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1050, 0.26).
narrative_ontology:measurement(feud_tr_t1100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1100, 0.31).
narrative_ontology:measurement(feud_tr_t1150, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1150, 0.35).
narrative_ontology:measurement(feud_tr_t1200, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1200, 0.39).
narrative_ontology:measurement(feud_tr_t1250, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1250, 0.42).
narrative_ontology:measurement(feud_tr_t1300, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1300, 0.45).

% Extraction over time
narrative_ontology:measurement(feud_be_t900, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 900, 0.66).
narrative_ontology:measurement(feud_be_t950, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 950, 0.7).
narrative_ontology:measurement(feud_be_t1000, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1000, 0.74).
narrative_ontology:measurement(feud_be_t1050, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1050, 0.79).
narrative_ontology:measurement(feud_be_t1100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1100, 0.82).
narrative_ontology:measurement(feud_be_t1150, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1150, 0.8).
narrative_ontology:measurement(feud_be_t1200, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1200, 0.77).
narrative_ontology:measurement(feud_be_t1250, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1250, 0.74).
narrative_ontology:measurement(feud_be_t1300, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1300, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t900, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 900, 0.38).
narrative_ontology:measurement(feud_su_t950, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 950, 0.43).
narrative_ontology:measurement(feud_su_t1000, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1000, 0.49).
narrative_ontology:measurement(feud_su_t1050, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1050, 0.55).
narrative_ontology:measurement(feud_su_t1100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1100, 0.62).
narrative_ontology:measurement(feud_su_t1150, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1150, 0.68).
narrative_ontology:measurement(feud_su_t1200, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1200, 0.74).
narrative_ontology:measurement(feud_su_t1250, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1250, 0.79).
narrative_ontology:measurement(feud_su_t1300, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1300, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'blood feud' conflates three structurally distinct claims about one standing arrangement. The stateless_coordination_reading treats the obligation as a coordination institution (low extraction, rope-shaped); the christianized_pacification_reading treats it as a violence-authority violation (victim set relocated to all vengeance-takers, beneficiary shifted to ecclesiastical authority); this extraction_cycle_reading treats it as a depletion cycle feeding royal consolidation (high extraction on kin economies, crown as capturer). Per the epsilon-invariance principle these are three constraints, not one constraint viewed from angles: each carries its own epsilon, its own stakeholder surface, its own classification, and each links the others here. The upstream/downstream structure runs from this reading outward: its diagnosis supplied the policy justification that shrank the coordination reading's empirical domain, while the christianized reading supplied the moral register in which the same suppression was preached.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
