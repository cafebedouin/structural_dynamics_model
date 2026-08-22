% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Prohibition Held as Irrevocable Divine-Natural Law (Immutable Mandate Reading)
 *   domain: constitutional/political-history
 *
 * SUMMARY:
 *   The salic_prohibition kernel — the bar on succession through the female
 *   line embedded in European dynastic constitutions — is contested among
 *   three readings. This file instantiates the immutable_mandate_reading: the
 *   prohibition held as irrevocable natural/divine law, woven into the
 *   dynastic constitution itself, under which female heirs are categorically
 *   ineligible, challengers to female succession act legitimately, and
 *   preventive war to secure agnatic priority is arguable. Family
 *   decomposition (epsilon-invariance): the colloquial label 'Salic Law'
 *   conflates three structurally distinct claims with different epsilon
 *   values. This reading authors epsilon at 0.60 for the standing
 *   agnatic-exclusion arrangement as it operates under immutable-mandate
 *   enforcement — categorical dispossession of female lines plus recurrent
 *   war externalities, damped by the real succession-clarity value any fixed
 *   order supplies. The sovereign_override_reading authors lower epsilon for
 *   the same referent (a reformable positive law that channels disputes into
 *   legislation rather than war), and the cognatic_reversion_reading authors
 *   a different victim set altogether (non-Frankish female lines were never
 *   bound, hence never victims). The readings are separate files linked by
 *   network.affects_constraints; this reading's six-century dominance shaped
 *   the terrain on which both siblings emerged as counter-claims. The
 *   claim/metric gap is deliberate: claimed_type=mountain is this reading's
 *   constitutive assertion (the constraint IS the claim of irrevocable
 *   natural/divine law — without it the reading collapses into a sibling),
 *   while the authored metrics describe the arrangement's documented
 *   operation; the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - - incumbent_male_monarchs: Agenda-setter and beneficiary (institutional/identity_locked) — enforce and embody the doctrine their own titles rest on
 *   - - collateral_male_dynasts: Primary beneficiary (powerful/constrained) — collect thrones when direct male lines fail; patronize the doctrine's defenders
 *   - - dynastic_jurists_theologians: Doctrinal entrepreneurs and secondary administrators (organized/identity_locked) — manufacture and transmit the natural-law framing
 *   - - ecclesiastical_arbiters: Sacralizing beneficiary (institutional/identity_locked) — crown kings, adjudicate marriages and successions, threaten spiritual sanctions
 *   - - female_dynastic_descendants: Primary target (powerless/trapped) — categorically excluded by birth, with no procedural path to eligibility
 *   - - female_line_claimants: Armed challengers and targets (powerful/constrained) — press claims through female ancestors by court, treaty, and war
 *   - - war_burdened_subject_populations: Diffuse cost-bearers (powerless/trapped) — finance and suffer the enforcement wars
 *   - - european_great_powers: Enforcement-cost bearers (institutional/mobile) — pulled into every succession crisis by balance-of-power logic
 *   - - constitutional_historians: Analytical observer (analytical/analytical) — reconstruct the doctrine's manufacture and breaches from the archive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.6).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.5).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.63).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.63).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Prohibition Held as Irrevocable Divine-Natural Law (Immutable Mandate Reading)").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/political-history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).
domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'b8a3b6af-ca57-45c4-8665-d583b60be4f2').
narrative_ontology:cs_kernel_codification('b8a3b6af-ca57-45c4-8665-d583b60be4f2', fixed_text).
narrative_ontology:cs_authority_grounding('b8a3b6af-ca57-45c4-8665-d583b60be4f2', lineage).
narrative_ontology:cs_interpretation_layer_present('b8a3b6af-ca57-45c4-8665-d583b60be4f2').
narrative_ontology:cs_reading_relation('b8a3b6af-ca57-45c4-8665-d583b60be4f2', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('b8a3b6af-ca57-45c4-8665-d583b60be4f2', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('b8a3b6af-ca57-45c4-8665-d583b60be4f2', foundational, agnatic_exclusion_divinely_ordained).
narrative_ontology:cs_axiom_status(agnatic_exclusion_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('b8a3b6af-ca57-45c4-8665-d583b60be4f2', agnatic_exclusion_divinely_ordained, theological).
narrative_ontology:cs_axiom('b8a3b6af-ca57-45c4-8665-d583b60be4f2', foundational, succession_order_irrevocable_by_human_authority).
narrative_ontology:cs_axiom_status(succession_order_irrevocable_by_human_authority, holdable).
narrative_ontology:cs_axiom_grounding('b8a3b6af-ca57-45c4-8665-d583b60be4f2', succession_order_irrevocable_by_human_authority, deontological).
narrative_ontology:cs_reference_frame('b8a3b6af-ca57-45c4-8665-d583b60be4f2', divine_natural_agnatic_order).
narrative_ontology:cs_drift_state('b8a3b6af-ca57-45c4-8665-d583b60be4f2', contemporary_constitutional_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('b8a3b6af-ca57-45c4-8665-d583b60be4f2', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, incumbent_male_monarchs).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, collateral_male_dynasts).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, dynastic_jurists_theologians).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, ecclesiastical_arbiters).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_dynastic_descendants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_line_claimants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, war_burdened_subject_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, european_great_powers).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, agnatic_primogeniture_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, divine_ordination_of_hereditary_succession).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, dynastic_constitution_inviolability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reign under and enforce the rule: they swear coronation oaths upholding the ancient succession order, decide disputes over eligibility, and punish challenges. Their own title rests on the doctrine — a king who conceded the rule could be revised would reopen questions about his own line's title. Leaving the arrangement would mean delegitimizing the throne he occupies.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, incumbent_male_monarchs, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, incumbent_male_monarchs, beneficiary).

% Cadet princes and their houses stand next in line whenever a reigning king lacks sons. When a direct male line fails, the rule hands them the crown a daughter or a sister's son would otherwise take — the Valois in 1328, the Bourbons in 1589. They patronize jurists and chroniclers who defend the rule and arrange marriages to keep male lines alive. Their exit is limited: rank, income, and marriage prospects are defined by their place in the male line.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, collateral_male_dynasts, beneficiary,
    powerful, generational, constrained, continental).

% Learned lawyers, chroniclers, and doctors of theology supply the doctrine's intellectual machinery: they trace the rule to Frankish antiquity, reconcile it with scripture and canon law, staff the courts and councils that adjudicate eligibility, and write the treatises that train each generation of officials. Their careers, chairs, and offices depend on the doctrine remaining authoritative and on their being its authorized interpreters.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, dynastic_jurists_theologians, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, dynastic_jurists_theologians, agenda_setter).

% Church authorities crown kings, pronounce on the lawfulness of marriages and successions, and threaten spiritual penalties against supporters of excluded claimants. Sacralizing the succession order is a principal source of the Church's leverage over temporal rulers; abandoning that role would surrender a pillar of ecclesiastical authority.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, ecclesiastical_arbiters, beneficiary,
    institutional, generational, identity_locked, continental).

% Daughters, sisters, and their children of royal houses are barred from succession by birth, with no procedure by which eligibility could ever attach to them. They receive dowries and appanages at best, watch collateral males take thrones their own descent qualifies them for, and can contest their exclusion only through male guardians, husbands, or sons willing to press claims. There is no exit from the category: the bar attaches to sex and birth order, not to anything they chose.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_dynastic_descendants, payer,
    powerless, generational, trapped, continental).

% Descendants who press claims through a female ancestor — a grandson through a king's daughter, a husband asserting a wife's right — carry the dispute into courts, treaties, and battlefields. Some command kingdoms of their own and can wage long wars over recognition, as the English crown did against the Valois after 1328. They can abandon a claim for compensation or alliance value, but renunciation is negotiated under pressure and rarely final.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_line_claimants, payer,
    powerful, biographical, constrained, continental).

% Peasants, towns, and taxpayers of the realms involved finance and man the armies that succession disputes require, and absorb the devastation of campaigns fought over who reigns. They have no seat in the councils that fix the succession order and no way to decline the costs.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, war_burdened_subject_populations, payer,
    powerless, generational, trapped, national).

% Neighboring crowns and their alliances are pulled into every major succession crisis, since a change of dynasty reshapes the balance of power. They underwrite claimants, sign partition treaties, and fight the wars that enforcement or challenge requires; they can sometimes decline intervention, but repeated crises make standing aloof costly.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, european_great_powers, payer,
    institutional, generational, mobile, continental).

% Modern scholars reconstruct how the doctrine was made: the drafting of the exclusions, the invention of antiquity for them, the interests they served, and the moments they were breached or abandoned. They hold the archives, the comparative law, and no stake in any living claim.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, collateral_male_dynasts).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes an unconditional, pre-agreed order of succession to the throne, so that no interregnum requires the estates, magnates, or electors to negotiate or choose among eligible candidates; every succession crisis has a determinate answer before it arises.
% TRANSFER_FUNCTION: Moves sovereign legitimacy, dynastic property, and the right to command along male lines only; in each crisis it transfers eligibility from female heirs and their descendants to the nearest male collateral, and transfers the costs of settling disputes — litigation, treaty concessions, war — onto claimants, realms, and populations.
% ABSENT_VOICES: Women of the dynasties whose eligibility the doctrine decides had no seat in the councils, parlements, chapters, or consistories where it was articulated; tax-paying populations who financed the enforcement wars were unrepresented; female-line descendants appear in the record almost exclusively as challengers to be answered, never as framers of the rule.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would void the collateral claims that took thrones in 1328, 1589, and 1700, restore eligibility to female lines across the European dynastic network, idle the juristic-ecclesiastical apparatus that maintains the doctrine, and remove the standard by which challengers to female succession claim legitimacy and preventive war is argued to be just.
% FOUNDING_PROBLEM: The fragility of elective and partible succession: crowns won by election invited magnate bargaining and civil war at every death, and partitions fragmented realms. The arrangement was built to make succession automatic and indisputable by fixing an unconditional order in advance.
% FOUNDING_PROBLEM_CORROBORATION: Non-beneficiary attestation exists for the problem itself: the deliberations of French royal counsellors in 1316-1328, recorded in chronicles and parlement registers, invoke the realm's peril from disputed succession, and English and Iberian records show the same certainty motive cited by parties with opposite interests in the exclusion. No source outside the male-line beneficiary set attests that the problem required the female exclusion specifically rather than any fixed order — modern constitutional historiography uniformly reads the exclusion as the contingent victory of one interested reading of the certainty requirement.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.60 at interval end, peaking 0.78 in 1830) because the rule dispossesses an entire class of heirs categorically and generates recurrent war externalities, while retaining genuine succession-clarity value that damps it below pure-extraction levels. Suppression (0.50 at end, peaking 0.84 in 1830) tracks enforcement intensity: doctrinal policing, censorship of succession debate, spiritual sanctions, and finally civil war — the arrangement never ran on voluntary compliance alone. Theater rises monotonically (0.28 to 0.63) as the rule's live function atrophied and its ceremonial, genealogical, and legendary apparatus (the invented Frankish antiquity, consecration rites, legitimist commemoration) grew relative to work done. Accessibility_collapse is 0.62, deliberately below mountain grade: alternatives stayed visible and were occasionally enacted (the Pragmatic Sanctions, the 1830 Spanish reversal, eventual cognatic reforms) — which is itself evidence against the reading's own immutability claim. Resistance is 0.68: six centuries of armed and litigious challenge from excluded lines. Assumptions stated: the interval is anchored to documented doctrinal events (1316 exclusion of Jeanne, 1450 post-Hundred Years' War entrenchment, 1589 Bourbon accession, 1713 Utrecht renunciations, 1789 ancien-regime apex, 1830 Carlist rupture, 1883 legitimist attrition, 2013 residual footprint); metric values are historian-indexed judgments of the arrangement's operation, not the reading's self-assessment. All series run on one shared eight-point grid; end-state scalars equal the 2013 values. Suppression is authored as a raw structural property, unscaled by power or scope; only extractiveness is engine-scaled.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the incumbent and collateral male seats the arrangement is a sacred, self-evident order — their titles, marriages, and posterity are constituted by it, and its immutability is experienced as the condition of political peace. From the female-line seats the same structure is total dispossession enforced by arms: eligibility denied by birth, claims answerable only by war or purchased renunciation. The juristic and ecclesiastical seats experience it as vocation and office — career and authority fused with the doctrine's authority. The engine computes these divergent per-seat classifications from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (incumbent monarchs, collateral dynasts, jurists, ecclesiastics) derive directionality near the beneficiary end — the arrangement subsidizes them; identity_locked exits among incumbents, jurists, and ecclesiastics fuse their positions with the doctrine and deepen the subsidy. Declared victims derive near the target end: trapped female dynastic descendants sit nearest the full-target pole (no exit from the category at all); powerful female-line claimants sit slightly less deep (resources to contest, negotiated renunciations possible); trapped subject populations bear diffuse war costs at high d; mobile great powers damp furthest among victims, since intervention is choosable even if rarely declined. Continental scope raises verification difficulty and modestly amplifies effective extraction engine-side. No directionality overrides were needed: role plus exit data yields the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — automatic, undisputed succession — remains real wherever hereditary sovereignty persists, but this mandate's specific content (female exclusion as divine law) has outlived its necessity: every fixed order solves the certainty problem, and most realms replaced agnatic exclusivity with reformed orders at far lower cost than the wars spent defending the original. Classification discipline cuts both ways: the reading's mountain claim must not be waved through as natural law (identifiable beneficiaries and six centuries of resistance mark it as constructed — the false-summit question is lodged as an omega), nor may the arrangement be flattened to pure extraction (succession clarity is a genuine collective-action good that any fixed order supplies). The structural data support a hybrid: real coordination function, heavy asymmetric extraction, active enforcement, with the immutability doctrine functioning as the enforcement multiplier. The R5 interview supplies the mismatch consumer's inputs: founding_problem_status=contested against disappearance_verdict=world_rearranges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_constructed_interest,
    'Is the agnatic prohibition a genuine natural or divine law (as this reading asserts), or a constructed dynastic rule whose natural-law framing serves identifiable male-line interests?',
    'Comparative and documentary analysis: jurisdictional variation in succession law among contemporaneous Christian kingdoms, the documented drafting history of the exclusions (1316, 1328), and the traceable invention of Frankish antiquity for the rule.',
    'If constructed, the false-summit signature reclassifies the claimed mountain toward a hybrid coordination/extraction type, and the immutability doctrine reads as interest-serving ideology rather than discovered law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_constructed_interest, empirical, 'Whether the prohibition''s natural-law character is real or manufactured.').

omega_variable(
    kernel_reading_structural_delta,
    'Which structural features of this story are properties of the immutable_mandate_reading rather than of the salic_prohibition kernel itself — what would change under the sibling readings?',
    'Generate the sibling stories (salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading) and diff epsilon, victim sets, and computed types against this file.',
    'Under sovereign_override_reading, suppression and extractiveness fall (a reformable positive law channels disputes into legislation rather than war); under cognatic_reversion_reading, the victim set contracts sharply, since non-Frankish female lines were never bound and therefore never victims at all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Reading-indexed versus kernel-fixed structure in the salic_prohibition family.').

omega_variable(
    modal_status_disagreement_locus,
    'Is the kernel contest located in the prohibition''s modal status (irrevocable divine law vs revocable positive law vs non-binding custom) rather than in preferences about female rule?',
    'Code the parties'' surviving arguments: if each party''s case turns on the law''s binding modality (its source, what could revoke it) rather than on outcomes for women, the contest is modal.',
    'Modal-status contests produce mutual foreclosure among readings within any single framework; preference contests would leave the readings coexisting — the relation typing in cs_structure depends on this determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modal_status_disagreement_locus, conceptual, 'Locating the salic kernel disagreement in modal status versus outcome preference.').

omega_variable(
    preventive_war_entailment,
    'Does the justification of preventive war to enforce agnatic priority follow necessarily from irrevocability, or is it an auxiliary commitment added by interested parties?',
    'Compare juristic and theological texts: passages deriving enforcement duties from the law''s divine status versus passages justifying war on prudential or dynastic-interest grounds.',
    'If the entailment holds, the reading''s suppression profile is intrinsic and high; if auxiliary, part of the measured suppression reflects bearer interests separable from the reading itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preventive_war_entailment, conceptual, 'Whether war-justification is entailed by the immutable-mandate premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 1316, 2013).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_immutable_tr_t1316, salic_prohibition__immutable_mandate_reading, theater_ratio, 1316, 0.28).
narrative_ontology:measurement_basis(salic_immutable_tr_t1316, observed).
narrative_ontology:measurement(salic_immutable_tr_t1450, salic_prohibition__immutable_mandate_reading, theater_ratio, 1450, 0.34).
narrative_ontology:measurement_basis(salic_immutable_tr_t1450, observed).
narrative_ontology:measurement(salic_immutable_tr_t1589, salic_prohibition__immutable_mandate_reading, theater_ratio, 1589, 0.4).
narrative_ontology:measurement_basis(salic_immutable_tr_t1589, observed).
narrative_ontology:measurement(salic_immutable_tr_t1713, salic_prohibition__immutable_mandate_reading, theater_ratio, 1713, 0.46).
narrative_ontology:measurement_basis(salic_immutable_tr_t1713, observed).
narrative_ontology:measurement(salic_immutable_tr_t1789, salic_prohibition__immutable_mandate_reading, theater_ratio, 1789, 0.5).
narrative_ontology:measurement_basis(salic_immutable_tr_t1789, observed).
narrative_ontology:measurement(salic_immutable_tr_t1830, salic_prohibition__immutable_mandate_reading, theater_ratio, 1830, 0.54).
narrative_ontology:measurement_basis(salic_immutable_tr_t1830, observed).
narrative_ontology:measurement(salic_immutable_tr_t1883, salic_prohibition__immutable_mandate_reading, theater_ratio, 1883, 0.6).
narrative_ontology:measurement_basis(salic_immutable_tr_t1883, observed).
narrative_ontology:measurement(salic_immutable_tr_t2013, salic_prohibition__immutable_mandate_reading, theater_ratio, 2013, 0.63).
narrative_ontology:measurement_basis(salic_immutable_tr_t2013, observed).

% Extraction over time
narrative_ontology:measurement(salic_immutable_be_t1316, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1316, 0.64).
narrative_ontology:measurement_basis(salic_immutable_be_t1316, observed).
narrative_ontology:measurement(salic_immutable_be_t1450, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1450, 0.7).
narrative_ontology:measurement_basis(salic_immutable_be_t1450, observed).
narrative_ontology:measurement(salic_immutable_be_t1589, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1589, 0.73).
narrative_ontology:measurement_basis(salic_immutable_be_t1589, observed).
narrative_ontology:measurement(salic_immutable_be_t1713, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1713, 0.76).
narrative_ontology:measurement_basis(salic_immutable_be_t1713, observed).
narrative_ontology:measurement(salic_immutable_be_t1789, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1789, 0.77).
narrative_ontology:measurement_basis(salic_immutable_be_t1789, observed).
narrative_ontology:measurement(salic_immutable_be_t1830, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1830, 0.78).
narrative_ontology:measurement_basis(salic_immutable_be_t1830, observed).
narrative_ontology:measurement(salic_immutable_be_t1883, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1883, 0.68).
narrative_ontology:measurement_basis(salic_immutable_be_t1883, observed).
narrative_ontology:measurement(salic_immutable_be_t2013, salic_prohibition__immutable_mandate_reading, base_extractiveness, 2013, 0.6).
narrative_ontology:measurement_basis(salic_immutable_be_t2013, observed).

% Suppression requirement over time
narrative_ontology:measurement(salic_immutable_su_t1316, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1316, 0.55).
narrative_ontology:measurement_basis(salic_immutable_su_t1316, observed).
narrative_ontology:measurement(salic_immutable_su_t1450, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1450, 0.62).
narrative_ontology:measurement_basis(salic_immutable_su_t1450, observed).
narrative_ontology:measurement(salic_immutable_su_t1589, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1589, 0.68).
narrative_ontology:measurement_basis(salic_immutable_su_t1589, observed).
narrative_ontology:measurement(salic_immutable_su_t1713, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1713, 0.74).
narrative_ontology:measurement_basis(salic_immutable_su_t1713, observed).
narrative_ontology:measurement(salic_immutable_su_t1789, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1789, 0.8).
narrative_ontology:measurement_basis(salic_immutable_su_t1789, observed).
narrative_ontology:measurement(salic_immutable_su_t1830, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1830, 0.84).
narrative_ontology:measurement_basis(salic_immutable_su_t1830, observed).
narrative_ontology:measurement(salic_immutable_su_t1883, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1883, 0.7).
narrative_ontology:measurement_basis(salic_immutable_su_t1883, observed).
narrative_ontology:measurement(salic_immutable_su_t2013, salic_prohibition__immutable_mandate_reading, suppression_requirement, 2013, 0.5).
narrative_ontology:measurement_basis(salic_immutable_su_t2013, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% 'Salic Law' decomposes into three epsilon-distinct constraints: immutable_mandate (this file — epsilon 0.60, victims are female lines across Christendom, enforcement culminates in war), sovereign_override (lower epsilon — a revocable positive law channels disputes into legislation), and cognatic_reversion (a different victim set — non-Frankish female lines were never bound). Linked as a constraint family via affects_constraints; the immutable reading is the historically dominant form against which both siblings defined themselves, and its collapse is the upstream condition for theirs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
