% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Divine Mandate Reading of Ma'at: Royal Embodiment and Unaccountable Mediation
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Ma'at kernel: the divine
 *   mandate reading, in which Ma'at flows from cosmic divine order
 *   exclusively through the living king to society, and the king — as Ma'at's
 *   embodiment — cannot violate it by definition. The standing arrangement
 *   under contest is New Kingdom pharaonic rule (surplus taxation, seasonal
 *   corvée, monumental labor conscription) legitimated as cosmic necessity.
 *   The kernel decomposes into three structurally distinct constraints with
 *   distinct epsilon values: this reading (epsilon 0.80 — the
 *   unaccountability move converts the entire surplus flow into uncapped,
 *   self-certifying taking, with rival readings suppressed as chaos); the
 *   reciprocity reading (moderate epsilon — royal obligation is binding, so
 *   the flow is capped by enforceable duty and the king becomes a potentially
 *   liable party); and the distributed maintenance reading (low epsilon —
 *   responsibility diffuses across stations, no single capturer exists). The
 *   influence runs from this reading to its siblings: the divine mandate
 *   reading was the official ideology whose suppression machinery kept the
 *   sibling readings from becoming operative constraints, and each sibling
 *   file links back here via network.affects_constraints. KEY AGENTS (by
 *   structural relationship): see key_agents.
 *
 * KEY AGENTS:
 *   - pharaoh_living_horus: Agenda-setting source-seat (institutional/arbitrage) — defines what Ma'at requires, stands outside the arrangement he proclaims, receives the surplus
 *   - amun_priesthood_establishment: Secondary beneficiary and co-administrator (institutional/identity_locked) — certifies reigns, collects temple revenues, absorbs drift through interpretation
 *   - scribal_administrative_class: Beneficiary middle layer (organized/constrained) — operates census, tax, and corvée machinery; owns the record
 *   - nile_valley_peasant_holders: Primary target (powerless/constrained) — bears surplus taxation in kind
 *   - corvee_conscript_workmen: Primary target (powerless/trapped) — bears seasonal labor conscription; struck at Deir el-Medina when rations failed
 *   - regional_cult_priesthoods: Secondary target (organized/constrained) — endowments absorbed, autonomy subordinated
 *   - unrecorded_village_majority: Excluded seat (powerless/trapped) — bore the arrangement but left no record
 *   - egyptological_analysts: Analytical observer (analytical/analytical) — sees the full structure from outside the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.8).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.74).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, snare).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Divine Mandate Reading of Ma'at: Royal Embodiment and Unaccountable Mediation").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, 'f818da51-b94c-43bf-9a9e-9b017b7188a5').
narrative_ontology:cs_kernel_codification('f818da51-b94c-43bf-9a9e-9b017b7188a5', formalized).
narrative_ontology:cs_authority_grounding('f818da51-b94c-43bf-9a9e-9b017b7188a5', extraction).
narrative_ontology:cs_interpretation_layer_present('f818da51-b94c-43bf-9a9e-9b017b7188a5').
narrative_ontology:cs_reading_relation('f818da51-b94c-43bf-9a9e-9b017b7188a5', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('f818da51-b94c-43bf-9a9e-9b017b7188a5', maat_order_principle__distributed_maintenance_reading, forecloses).
narrative_ontology:cs_axiom('f818da51-b94c-43bf-9a9e-9b017b7188a5', foundational, royal_embodiment_precludes_violation).
narrative_ontology:cs_axiom_status(royal_embodiment_precludes_violation, holdable).
narrative_ontology:cs_axiom_grounding('f818da51-b94c-43bf-9a9e-9b017b7188a5', royal_embodiment_precludes_violation, theological).
narrative_ontology:cs_axiom('f818da51-b94c-43bf-9a9e-9b017b7188a5', secondary, surplus_flow_constitutes_cosmic_maintenance).
narrative_ontology:cs_axiom_status(surplus_flow_constitutes_cosmic_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('f818da51-b94c-43bf-9a9e-9b017b7188a5', surplus_flow_constitutes_cosmic_maintenance, instrumental).
narrative_ontology:cs_reference_frame('f818da51-b94c-43bf-9a9e-9b017b7188a5', pharaonic_exclusive_mediation).
narrative_ontology:cs_drift_state('f818da51-b94c-43bf-9a9e-9b017b7188a5', late_new_kingdom_oracle_certification, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f818da51-b94c-43bf-9a9e-9b017b7188a5', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaoh_living_horus).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, amun_priesthood_establishment).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, scribal_administrative_class).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, nile_valley_peasant_holders).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, corvee_conscript_workmen).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, regional_cult_priesthoods).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, pharaonic_cosmic_mediation_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, royal_infallibility_by_definition).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, ma_at_as_exclusive_royal_channel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rules as the living Horus, son of Ra. Proclaims what Ma'at requires and receives the Two Lands' surplus as the offering that sustains ordered existence. Because he embodies the order he administers, no failure of his can count as a failure of Ma'at — droughts, defeats, and shortages are reattributed to officials, enemies, or a temporary surge of chaos. Demonstrated at Amarna that the theological framework itself could be rewritten by royal decree. Exit is meaningless from this seat: he is the framework's source, and the valley's surplus lands at his household and the temples he favors.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaoh_living_horus, agenda_setter,
    institutional, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, pharaoh_living_horus, beneficiary).

% Staffs the state cult of Amun: daily offerings, oracles, festivals, and the doctrinal interpretation that certifies each reign. Holds vast temple estates fed by royal endowment and tithe. Its authority rests on the framework it administers, so abandoning the framework would dissolve it; by the interval's end its oracles effectively choose kings, and its high priest rules Upper Egypt in all but name.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, amun_priesthood_establishment, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, amun_priesthood_establishment, agenda_setter).

% Operates the census, tax assessment, granary accounts, and corvée rolls that move the valley's surplus upward. Literacy is their monopoly and their livelihood; status, exemption from manual labor, and career depend on the arrangement running. They also wrote the surviving record — grievance enters their texts mainly as administrative anomaly, as with the workmen's strike they recorded.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, scribal_administrative_class, beneficiary,
    organized, biographical, constrained, national).

% Farm small plots, owe a share of grain and livestock to the state, and supply conscripts each season. Flight to temple asylum or the frontier is possible but means abandoning land and kin; most stay and pay. Their view of the arrangement survives only indirectly, in laments and wisdom texts written by others.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, nile_valley_peasant_holders, payer,
    powerless, biographical, constrained, national).

% Drafted in rotating crews for royal tombs, temples, and quarries; the Deir el-Medina crew cut the Valley of the Kings tombs for rations paid by the state. When rations stopped arriving in year 29 of Ramesses III they sat down at the mortuary temple gates — the first recorded labor strike — received arrears, and struck repeatedly as shortfalls recurred. No exit from the obligation existed; only the withholding of their labor.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, corvee_conscript_workmen, payer,
    powerless, immediate, trapped, national).

% Serve local gods at Abydos, Memphis, Elephantine, and elsewhere. Over the New Kingdom their endowments were redirected toward Amun's estate, their festivals subordinated to the royal calendar, and their appointments made subject to central confirmation. They kept their rites and local standing at the price of subordination.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, regional_cult_priesthoods, payer,
    organized, generational, constrained, regional).

% The illiterate majority whose labor and grain carried the arrangement. Nothing they said survives; literacy belonged to the households that administered the taking. They appear statistically — in census fragments and ration lists — and as silence.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, unrecorded_village_majority, excluded,
    powerless, biographical, trapped, national).

% Reconstruct the arrangement from papyri, inscriptions, settlement archaeology, and the strike record. Stand wholly outside the framework; theirs is the only seat from which the full structure — flows, certifications, suppressions, and the strike — is visible at once.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, egyptological_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaoh_living_horus).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Answers, once and for the whole valley, who may organize flood-response labor, granary storage, temple building, and royal succession: the king whose body mediates Ma'at. Centralized mobilization for embankments, canals, and famine relief is licensed by the same doctrine that moves surplus to the crown; succession disputes are settled by the fact that only the reigning king can be the order's source.
% TRANSFER_FUNCTION: Moves grain, livestock, textiles, and seasonal labor from peasant holdings and conscript crews, through scribal assessment and granary accounting, to the royal household and the temple estates the crown endows — justified as the offering without which ordered existence fails.
% ABSENT_VOICES: The unrecorded village majority would object and cannot: literacy was monopolized by the beneficiary scribal class, so their objection survives only as silence and as the one strike the scribes found undeniable. Holders of rival readings — reciprocity-shaped obligations, distributed responsibility — appear in the record mostly as the objects of suppression, notably the proscription of Amun's name during the Amarna years.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, every reign would lose its legitimacy warrant at a stroke: succession would become open contest among claimants and army factions, corvée and tax collection would lose their warrant and collapse into bargaining or force, and the temple economy built on royal endowment would lose its charter. The New Kingdom state as constituted could not run a season without it; something functionally equivalent would have to be rebuilt before mobilization resumed.
% FOUNDING_PROBLEM: After unification, the valley needed a warrant for concentrating labor and surplus above the village level and for settling succession without recurring civil war. The flow-through doctrine supplied it: obey and feed this man, because ordered existence itself passes through him.
% FOUNDING_PROBLEM_CORROBORATION: From outside the beneficiary set: the Turin strike papyrus attests that at the workface the arrangement ran on rations delivered, not conviction — when provisioning failed, the cosmic warrant bought nothing. The Admonitions of Ipuwer and later famine traditions attest that contemporaries imagined order collapsing when royal order failed, corroborating that the stakes-claim was load-bearing. Modern Egyptological synthesis (Kemp, Assmann) reconstructs the doctrine as state-legitimation technology rather than reportage. No source outside the beneficiary set attests that the problem required the unaccountability move specifically — that element is attested only by the doctrine's own beneficiaries.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.80 at interval end) because the surplus flow is uncapped by any accountability the framework recognizes — the rate is whatever the court declares cosmic order requires, and the definitional immunity removes appeal. Suppression (0.74) is structural: rival readings were prosecuted as chaos, most violently in the Amarna years (series spike to 0.90), when the machinery's full reach showed. Theater (0.55) reflects a ritual economy that was never mere show — calendar regulation, flood-cycle administration, and festival logistics were real — but whose share of purely performative maintenance of royal divinity grew as provisioning failed; the series rises from 0.28 to 0.55 with an Amarna spike to 0.56. Accessibility_collapse (0.48) is moderate: the sibling readings stayed live in the record, personal piety flourished, temple asylum gave partial refuge — but no one could exit the cosmology itself. Resistance (0.58) is documented, not inferred: the Deir el-Medina strikes, tomb-robbery waves, flight from levies, and the memory of fragmentation preserved in the laments. All three tracked series share one six-point grid (t = 0, 100, 180, 260, 340, 420, spanning roughly 1550–1130 BCE: Ahmose's restoration through the Ramesside collapse); the Amarna column is the reading exercising its defining prerogative — rewriting the kernel by decree — at maximum enforcement cost.
 *
 * PERSPECTIVAL GAP:
 *   From the royal seat the arrangement contains no taking at all: flows are offerings, and the definitional immunity means no state of the world can count as the king violating Ma'at — that seat is epistemically closed to the arrangement's burdens by the reading's own axiom. From the Amun seat the same structure is genuine coordination plus earned rent. From the payer seats it is uncompensated removal of grain and labor under a warrant that cannot be appealed. The engine computes these divergences from the structural data; the authored claim does not adjudicate them. The deepest gap is temporal: at t=0 the payer seats largely assented (restoration after Hyksos rule made the warrant credible); by t=420 the strike record shows assent replaced by arithmetic.
 *
 * DIRECTIONALITY LOGIC:
 *   The king sits nearest the beneficiary pole: he writes the rule, receives the flow, and holds arbitrage-grade position — Amarna proved the kernel itself was rewritable by decree. The Amun establishment shares the beneficiary pole but is identity_locked: its authority is the framework, so it rides the arrangement it helps certify, drifting toward co-agenda-setter as royal certification weakens. Scribes sit low-moderate: real rents (exemption, status, livelihood) against real service rendered. Peasant holders and conscript workmen sit nearest the target pole — constrained and trapped exit respectively amplify their effective burden; the strike shows the target seat testing the only lever it had. Regional cult priesthoods sit high-moderate: their endowments and autonomy were the flow's secondary channel. No directionality overrides were needed: the beneficiary/victim declarations plus the exit atoms derive these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a warrant for concentrated mobilization and settled succession — remains live in some form in every agrarian polity, but this reading's specific solution (certainty by definition, enforced by suppressing rival readings) had decayed into performance-plus-force by the interval's end: rituals continued at scale while rations failed, and oracles rather than royal embodiment chose kings. Reading the arrangement as pure coordination would launder the unaccountability move behind the real flood-control and granary successes; reading it as nothing but taking would erase the genuine coordination the doctrine performed. The authored classification with a declared resource_allocation coordination function keeps both facts in view: the coordination is real, its inherent floor is low, and the surplus flow runs far past it. The founding_problem_status x disappearance_verdict pair (contested x world_rearranges) records that the parties themselves dispute whether the warrant still does anything.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the maat_order_principle kernel — the divine_mandate_reading. What would change structurally if a sibling reading were instantiated instead?',
    'Compare the three instantiated constraint files: under reciprocity_reading the king becomes a potentially liable party and the flow is capped by enforceable duty; under distributed_maintenance_reading responsibility diffuses across stations and no single seat captures the flow.',
    'Under the reciprocity sibling the arrangement shifts toward hybrid coordination/extraction; under the distributed sibling it approaches pure coordination with diffuse costs. The disagreement is located in two elements: whether royal action is constrainable by Ma''at claims, and whether non-royal conduct has independent cosmic efficacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the Ma''at kernel; siblings would alter the victim set and the directionality profile.').

omega_variable(
    embodiment_claim_falsifiability,
    'Is ''the king cannot violate Ma''at'' unfalsifiable by design, or did the tradition operate de facto failure conditions (First Intermediate Period famines, the Hyksos interlude) that functioned as falsification?',
    'Trace how the tradition processed regime-level failure: reattribution to officials, enemies, or a temporary surge of chaos, versus any concession that royal Ma''at itself had failed.',
    'If de facto falsifiable, real accountability pressure operated inside the framework and the arrangement sits nearer hybrid coordination/extraction; if unfalsifiable, the definitional-immunity structure is confirmed and the authored classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodiment_claim_falsifiability, empirical, 'Whether the embodiment claim possessed operational failure conditions.').

omega_variable(
    suppression_structural_vs_internalized,
    'Was compliance sustained by external enforcement (corvée rolls, granary leverage, prosecution of rival readings) or by internalized cosmological conviction (personal piety, fear of chaos)?',
    'Track devotional-text production (personal piety expanded sharply after the Amarna years) against enforcement records, and observe conduct when enforcement capacity decayed at the Third Intermediate Period onset.',
    'If a large share was internalized, effective suppression exceeds the structural measure and the arrangement outlives its enforcement machinery, as it partly did; if structural, enforcement decay predicts rapid dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between enforcement machinery and internalized conviction.').

omega_variable(
    coordination_floor_separability,
    'How much of the surplus flow was the irreducible cost of the coordination the doctrine performed (flood response, granaries, monumental infrastructure) versus rent above that floor?',
    'Compare extraction intensity and delivery in periods of demonstrated coordination output (Sethos I''s canal works, Ramesses III''s famine relief) against pure monumental programs (Ramesses II''s late building).',
    'A large irreducible-cost share supports a hybrid reading; a small share confirms that the flow''s size was set by royal appetite wearing cosmic necessity as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_floor_separability, empirical, 'Separability of coordination cost from rent in the surplus flow.').

omega_variable(
    amarna_restructuring_interpretation,
    'Does Akhenaten''s unilateral replacement of Amun with the Aten prove the kernel was royal property — the source-not-subject structure operating as designed — or a violation revealing a latent constraint the reading denies?',
    'Analyze the restoration that followed: if the tradition treated the Amarna years as a lapse of Ma''at attributable to the king himself, a latent constraint existed; if it reattributed the episode to chaos or erased it, the kernel was indeed royal property.',
    'The royal-property reading confirms the authored structure; the latent-constraint reading transfers part of the accountability the reciprocity sibling claims, moving this reading toward hybrid territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amarna_restructuring_interpretation, conceptual, 'Whether the Amarna episode evidences royal ownership of the kernel or a latent binding constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 420).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__divine_mandate_reading, theater_ratio, 100, 0.32).
narrative_ontology:measurement(maat_tr_t180, maat_order_principle__divine_mandate_reading, theater_ratio, 180, 0.56).
narrative_ontology:measurement(maat_tr_t260, maat_order_principle__divine_mandate_reading, theater_ratio, 260, 0.42).
narrative_ontology:measurement(maat_tr_t340, maat_order_principle__divine_mandate_reading, theater_ratio, 340, 0.5).
narrative_ontology:measurement(maat_tr_t420, maat_order_principle__divine_mandate_reading, theater_ratio, 420, 0.55).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__divine_mandate_reading, base_extractiveness, 100, 0.64).
narrative_ontology:measurement(maat_be_t180, maat_order_principle__divine_mandate_reading, base_extractiveness, 180, 0.68).
narrative_ontology:measurement(maat_be_t260, maat_order_principle__divine_mandate_reading, base_extractiveness, 260, 0.73).
narrative_ontology:measurement(maat_be_t340, maat_order_principle__divine_mandate_reading, base_extractiveness, 340, 0.77).
narrative_ontology:measurement(maat_be_t420, maat_order_principle__divine_mandate_reading, base_extractiveness, 420, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__divine_mandate_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(maat_su_t180, maat_order_principle__divine_mandate_reading, suppression_requirement, 180, 0.9).
narrative_ontology:measurement(maat_su_t260, maat_order_principle__divine_mandate_reading, suppression_requirement, 260, 0.76).
narrative_ontology:measurement(maat_su_t340, maat_order_principle__divine_mandate_reading, suppression_requirement, 340, 0.72).
narrative_ontology:measurement(maat_su_t420, maat_order_principle__divine_mandate_reading, suppression_requirement, 420, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, resource_allocation).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Ma'at' conflates three structurally distinct claims about who maintains cosmic order and who is bound by it. This file holds the divine mandate reading (epsilon 0.80, unaccountable royal mediation, rival readings suppressed); maat_order_principle__reciprocity_reading holds the binding-obligation reading (moderate epsilon, king as potentially liable party); maat_order_principle__distributed_maintenance_reading holds the distributed-responsibility reading (low epsilon, no single capturer). The upstream/downstream link runs from this reading to its siblings because the divine mandate ideology was the official framework whose suppression machinery prevented the sibling readings from becoming operative; each sibling links back here. Per the epsilon-invariance principle, the three are separate constraints with separate beneficiaries, victims, and classifications — not one constraint viewed from angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
