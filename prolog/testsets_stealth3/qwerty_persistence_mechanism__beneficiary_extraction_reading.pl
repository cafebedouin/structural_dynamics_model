% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Maintenance Regime — Beneficiary Extraction Reading
 *   domain: economic_history/technology_studies
 *
 * SUMMARY:
 *   This story instantiates the beneficiary_extraction_reading of the QWERTY
 *   persistence kernel: the claim that the layout persisted because
 *   identifiable incumbents — the Remington/Union Typewriter complex and the
 *   commercial typing schools invested in it — actively maintained it against
 *   alternatives, protecting training investments and market position, with
 *   extraction arriving through artificially constructed switching costs. The
 *   reading asserts a specific structural fingerprint: a genuine coordination
 *   function (single-layout compatibility) WITH asymmetric extraction
 *   (manufacturer rents, school asset protection) AND active enforcement
 *   (patent control, school exclusivity contracts, channel refusal of rival
 *   layouts). Per the epsilon-invariance principle this is one of three
 *   sibling stories decomposing the colloquial label 'why QWERTY persisted':
 *   the naturalization_reading authors a no-beneficiary fair-competition
 *   constraint, and the lock_in_reading authors a no-agenda-setter
 *   coordination-failure constraint. All three are linked via
 *   network.affects_constraints; each carries its own epsilon, its own
 *   stakeholder surface, and its own classification. KEY AGENTS (by
 *   structural relationship): - incumbent_typewriter_manufacturers: agenda
 *   setter (institutional/arbitrage) — administers the regime, collects its
 *   rents, faces no exit cost - established_commercial_typing_schools:
 *   beneficiary (organized/constrained) — collects curriculum-asset
 *   protection, lends legitimacy - trained_office_typists: payer with
 *   secondary beneficiary position (moderate/identity_locked) — bears skill
 *   lock-in and foregone tools - office_equipment_buyers: payer with
 *   secondary beneficiary position (organized/constrained) — pays rents,
 *   receives interchangeability - rival_keyboard_layout_developers: payer
 *   (powerless/trapped) — the enforcement machinery's direct target -
 *   economic_path_dependence_historians: analytical observer — sees the full
 *   contested structure
 *
 * KEY AGENTS:
 *   - incumbent_typewriter_manufacturers: agenda setter (institutional/arbitrage) — controls patents, channels, and school contracts; captures the rents; exit is costless for them
 *   - established_commercial_typing_schools: beneficiary (organized/constrained) — curriculum capital protected by the regime; endorse and legitimize it
 *   - trained_office_typists: payer, secondary beneficiary (moderate/identity_locked) — universal employability gained, retraining risk and inferior tools borne
 *   - office_equipment_buyers: payer, secondary beneficiary (organized/constrained) — above-cost prices paid, interchangeability received; cannot convert fleets unilaterally
 *   - rival_keyboard_layout_developers: payer (powerless/trapped) — alternatives suppressed at every commercialization chokepoint
 *   - economic_path_dependence_historians: observer (analytical/analytical) — adjudicate nothing, see everything
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.62).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.7).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Maintenance Regime — Beneficiary Extraction Reading").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'cd8f91d9-c827-4229-95b1-66954f59474a').
narrative_ontology:cs_kernel_codification('cd8f91d9-c827-4229-95b1-66954f59474a', formalized).
narrative_ontology:cs_authority_grounding('cd8f91d9-c827-4229-95b1-66954f59474a', extraction).
narrative_ontology:cs_interpretation_layer_present('cd8f91d9-c827-4229-95b1-66954f59474a').
narrative_ontology:cs_reading_relation('cd8f91d9-c827-4229-95b1-66954f59474a', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_reading_relation('cd8f91d9-c827-4229-95b1-66954f59474a', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('cd8f91d9-c827-4229-95b1-66954f59474a', foundational, persistence_is_actively_produced_by_incumbent_defense).
narrative_ontology:cs_axiom_status(persistence_is_actively_produced_by_incumbent_defense, holdable).
narrative_ontology:cs_axiom_grounding('cd8f91d9-c827-4229-95b1-66954f59474a', persistence_is_actively_produced_by_incumbent_defense, empirically_contingent).
narrative_ontology:cs_axiom('cd8f91d9-c827-4229-95b1-66954f59474a', secondary, switching_costs_are_manufactured_not_spontaneous).
narrative_ontology:cs_axiom_status(switching_costs_are_manufactured_not_spontaneous, holdable).
narrative_ontology:cs_axiom_grounding('cd8f91d9-c827-4229-95b1-66954f59474a', switching_costs_are_manufactured_not_spontaneous, empirically_contingent).
narrative_ontology:cs_reference_frame('cd8f91d9-c827-4229-95b1-66954f59474a', actively_managed_standard_regime).
narrative_ontology:cs_drift_state('cd8f91d9-c827-4229-95b1-66954f59474a', post_liebowitz_margolis_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cd8f91d9-c827-4229-95b1-66954f59474a', '2026-08-06T09:15:00Z').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, established_commercial_typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, rival_keyboard_layout_developers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, trained_office_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_equipment_buyers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, trained_office_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_equipment_buyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the layout's patent estate, cross-licensing terms, and distribution channels; signs exclusive contracts with commercial typing schools; decides which alternative layouts ever reach manufacture. Collects above-cost margins sustained by the installed base of trained operators and interchangeable machines. Because they own the tooling, the channels, and the school partnerships, their exit from the arrangement is trivial — they could produce any layout tomorrow.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Built curricula, trained instructor corps, and purchased furniture and materials keyed to one layout; their course catalog's asset value depends on that layout remaining the employability standard. They publicly endorse the incumbent layout and lend legitimacy to its defense, collecting tuition from students guaranteed a portable skill. Retooling for another layout would strand their curriculum capital.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, established_commercial_typing_schools, beneficiary,
    organized, biographical, constrained, national).

% Sell QWERTY-specific motor skill as their marketable asset. Universal employability under a single standard benefits them — one training, any employer. But they carry the arrangement's costs: retraining risk if layouts diverge, foregone ergonomic improvements, and wage-side absorption of the efficiency losses of an inferior layout. Leaving means abandoning accumulated skill capital or paying for retraining themselves.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, trained_office_typists, payer,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, trained_office_typists, beneficiary).

% Large corporations and government offices purchasing machine fleets and hiring typing labor. They pay above-cost prices and forgo the productivity of demonstrably faster layouts, but benefit from brand-interchangeable parts, repair networks, and a deep temporary-labor pool trained on the same keyboard. Switching layouts means re-equipping fleets and re-training staff simultaneously — a coordinated move no single buyer can make alone.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_equipment_buyers, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_equipment_buyers, beneficiary).

% Inventors and licensees of alternative layouts (the Dvorak circle foremost) holding patents and efficiency evidence but unable to secure manufacturer production runs, dealer networks, or school adoption. Manufacturer refusal and school exclusivity contracts close every commercialization path; their exclusion is precisely what the enforcement machinery maintains. Exit means abandoning commercialization entirely.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, rival_keyboard_layout_developers, payer,
    powerless, biographical, trapped, national).

% Analyze the archival record — contract ledgers, trade-press coverage, school catalogs, patent litigation — from outside the arrangement. They see both the documented maintenance conduct and the counter-evidence cited by rival interpretations, and their disputes define the contested frame this reading sits inside.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_path_dependence_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single standardized key layout lets typists train once and work for any employer, lets schools teach one curriculum, lets buyers mix machine brands and hire interchangeable operators, and lets manufacturers amortize one tooling configuration. The compatibility problem across a fragmented manufacturing market is genuinely solved by everyone converging on one layout.
% TRANSFER_FUNCTION: Moves surplus from machine buyers and would-be adopters of faster layouts to the incumbent manufacturers (above-cost pricing sustained by manufactured switching costs), and preserves the asset value of incumbent school curricula and instructor corps at the expense of typists' access to better tools.
% ABSENT_VOICES: Rival layout developers were structurally excluded from every commercialization channel — manufacturer floors, dealer networks, school boards. Typists themselves were never consulted; the standard was negotiated among manufacturers, school proprietors, and large buyers. Both groups would have objected to the terms and had the relevant evidence.
% DISAPPEARANCE_RATIONALE: If the maintenance machinery — patent control, school exclusivity contracts, channel refusal — vanished overnight, alternative layouts would compete on measurable speed and fatigue, schools would retool curricula within a few hiring cycles, manufacturers would lose the rent stream the installed base secures, and buyer coalitions would coordinate fleet conversions. The arrangement's persistence depends on identifiable parties actively maintaining it.
% FOUNDING_PROBLEM: Early typewriter markets were fragmented across incompatible layouts and mechanisms; manufacturers needed scale and predictable repeat demand, schools needed a stable curriculum to sell, and buyers needed operator supply. The arrangement was built to solve skill-and-hardware compatibility across competing producers.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary trade-press archives and manufacturer catalogs attest the original fragmentation problem, corroborated by economic-historians working outside the benefiting parties. Whether the problem REMAINS live is disputed: the incumbent-manufacturer seat attests ongoing protection needs; the historian seats largely attest that compatibility was solved decades before the interval's end and that the surviving function is position protection. No living beneficiary attests the founding problem — the original firms are dissolved.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. CLAIMED TYPE tangled_rope follows from this reading's own structural premise: the reading asserts BOTH a real coordination function (layout compatibility across a fragmented market) AND asymmetric extraction (rents via manufactured switching costs) AND active enforcement — the exact triple the canonical classifier requires, and exactly what distinguishes this reading from its siblings (naturalization deletes the extraction half; lock_in deletes the enforcement half). METRICS are authored descriptively. Extractiveness 0.62: substantial rents, but bounded — competition entered the industry repeatedly, and part of buyer cost buys genuine interchangeability. Suppression 0.70 (raw, unscaled structural property): the regime's persistence depended on closing commercialization paths to rivals — contract exclusivity with schools, refusal to tool alternative layouts, patent-pool leverage — not on participant preference. Theater ratio 0.30: maintenance activity began as real function-building (curriculum standards, parts interchangeability) and accumulated a growing performative shell (endorsement rituals, standards-committee defenses aimed at protecting position rather than producing compatibility) — hence the rising series. Accessibility collapse 0.60: rival layouts remained technically learnable and manufacturable throughout — alternatives never vanished, they were choked at distribution — so collapse is substantial but visibly incomplete. Resistance 0.55: the Dvorak campaigns, buyer pressure, and the eventual scholarly contestation were real and sustained. The measurement series run on ONE SHARED GRID (every tracked metric authored at all seven decadal points 1880-1940) showing monotonic accumulation of extraction and enforcement through the trust-consolidation decades, plateauing by the 1920s with a slight Depression-era relaxation as entry (IBM electrics) pressed margins. The trajectory is accumulative rather than cyclical: enforcement ratcheted up as the installed base grew, because each year of training investment raised the cost of allowing exit. The interval-end values equal the base_properties scalars by construction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda_setter seat (incumbent manufacturers, arbitrage exit, generational horizon), the regime is a managed standard they built and rationally defend — coordination and position-protection indistinguishable, effective extraction near the subsidy end. From the payer seats the identical structure operates as enforced extraction with different textures: rival developers meet it as total exclusion (trapped, powerless, d near full target); buyers meet it as a price-and-productivity tax they cannot escape unilaterally (constrained, organized); typists meet it as skill lock-in (identity_locked) softened by the employability benefit of universality. Same-level differentiation matters: office_equipment_buyers and trained_office_typists hold similar nominal market weight but diverge on exit — buyers face a costly-but-possible coordinated conversion (constrained), typists face identity fusion between their skill and the standard itself (identity_locked), so the engine should place the typist seat nearer the target end than the buyer seat despite comparable power. Inter-institutionally, the manufacturer seat and the school seat are both organized actors but sit on opposite sides of the receipt line: the schools collect protection without administering anything, which is why their role is beneficiary, not agenda_setter.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation: incumbent manufacturers (agenda_setter, arbitrage exit, institutional power) sit nearest the beneficiary end — they collect the transfer and control the rules; typing schools (pure beneficiary, constrained) collect without running, low d; rival developers (payer, trapped, powerless) sit at the full-target end — the enforcement object itself; buyers (payer, constrained, organized) sit high-target with partial damping from their secondary interchangeability benefit. One explicit override is authored: trained_office_typists are declared as victims, which the raw derivation would push toward near-full target given identity_locked exit — but their secondary_role beneficiary position is real (one training, any employer; the universal standard is what makes their skill liquid), pulling their true d down to roughly 0.6. The override on the 'moderate' power atom encodes that correction; it is justified because the derivation chain cannot see secondary-role damping, only the primary victim declaration. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled by directionality and spatial scope in the engine's computation — the commentary accordingly treats the 0.70 suppression figure as an unscaled structural fact about channel closure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — skill/hardware compatibility across fragmented manufacturers — was effectively solved by the first decade of the twentieth century, when the layout became the industry's de facto universal; the arrangement nonetheless persisted and hardened through the interval's second half, its surviving function being position protection rather than compatibility production. Hence mandatrophy_resolved: true, authored independently of any metric value. The tangled_rope classification prevents mislabeling in both directions: reading the regime as pure rope (the naturalization sibling's move) would erase the documented extraction and enforcement; reading it as pure snare (extraction with coordination as mere cover) would erase the genuine compatibility function that gives the enforcement its grip — the switching costs bite only because the coordination is real. The piton failure mode is also guarded: the theater_ratio series rises but stays below dominance, because the maintenance activity retained functional content (fleet standardization, labor-pool depth) even as its defensive share grew. The R5 mismatch consumer reads founding_problem_status (contested) x disappearance_verdict (world_rearranges): the arrangement's persistence demonstrably depends on its parties, while the parties dispute whether what it still solves is the founding problem — a contested-genealogy profile, not a dead-mandate zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This file instantiates the beneficiary_extraction_reading of the kernel qwerty_persistence_mechanism. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Compare the three sibling stories'' beneficiary/victim declarations and enforcement gates: the naturalization_reading deletes beneficiaries and victims (fair competition, no capturer — rope-profile epsilon near zero); the lock_in_reading retains victims and switching costs but diffuses agency (no agenda_setter, no enforcement gate — coordination-failure profile). Disagreement is located in the causal attribution of persistence: deliberate incumbent maintenance versus spontaneous path-dependent dynamics versus selection adequacy.',
    'If the naturalization reading were adopted, this constraint''s beneficiaries and enforcement structure would be unfounded and the classification would collapse toward rope; if lock_in were adopted, the agenda_setter seat and suppression series would be unfounded and extraction would read as emergent rather than engineered. Each sibling is a separate constraint file; this story authors only its own reading''s structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this constraint is one reading of a contested kernel; sibling readings instantiate different beneficiary structures and different epsilon.').

omega_variable(
    suppression_evidence_attribution,
    'How much of the measured suppression reflects documented conduct (cross-licensing pacts, school exclusivity contracts, refusal of rival-layout production runs) versus retrospective narrative construction by advocates of the extraction story?',
    'Archival analysis of contract ledgers, trade-association correspondence, and patent-pool agreements from 1880-1940; count documented suppression episodes against the narrative''s claims.',
    'If documented conduct is thinner than the narrative assumes, suppression falls materially and the reading''s structure approaches the lock_in_reading''s coordination-failure profile with the enforcement gate unfounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_evidence_attribution, empirical, 'Whether the suppression metric is grounded in archival conduct or in reading-advocates'' reconstruction.').

omega_variable(
    rent_capture_durability,
    'Did incumbent rents from the layout regime survive competitive entry (Underwood''s rise, Royal, later IBM electrics), or did competition dissipate them?',
    'Profit-rate and price-series comparison between typewriter manufacturing and comparable durable-goods industries across the interval.',
    'If rents dissipated under entry, the extraction component of epsilon is overstated by this reading and the effective classification trends toward a coordination-dominated rope; if rents persisted, the extraction reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_capture_durability, empirical, 'Durability of incumbent rent capture under competitive pressure.').

omega_variable(
    counterfactual_superiority_magnitude,
    'How large was the real efficiency gap between QWERTY and its leading rival (the Dvorak layout), which calibrates how much of the imposed switching cost counted as artificial extraction rather than defensible transition friction?',
    'Controlled re-analysis of the 1930s efficiency studies (including the U.S. Navy trials) with modern replication standards, correcting for experimenter affiliation effects documented in later replications.',
    'A small true gap collapses the ''artificial switching costs'' basis of this reading''s epsilon toward zero; a large robust gap confirms substantial extraction via blocked adoption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_superiority_magnitude, empirical, 'Magnitude of rival-layout superiority, which sizes the artificial component of switching costs.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the kernel the layout artifact itself (a fixed encoding standard) or the maintenance REGIME organized around it (contracts, school alliances, channel control)? This story declares fixed_text/formalized codification with extraction-grounded authority; framing the kernel as the bare artifact would instead suggest distributed authority with no designated interpreter.',
    'Test which framing reproduces the observed adjudication structure: if disputes over the standard are settled by manufacturer-controlled bodies, the regime framing holds; if no body reliably adjudicates and outcomes follow decentralized adoption alone, the artifact framing holds.',
    'Under the artifact framing, authority_grounding shifts to distributed, interpretation_layer_present becomes invalid, and the cs_pattern classification changes; the beneficiary-extraction structure itself survives either framing, but the commitment-system signature differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Alternative framings of the kernel produce different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1880, 1940).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1880, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(qwer_tr_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1890, 0.14).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(qwer_tr_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1910, 0.22).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1920, 0.26).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1930, 0.29).
narrative_ontology:measurement(qwer_tr_t1940, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1940, 0.3).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1880, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1880, 0.44).
narrative_ontology:measurement(qwer_be_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(qwer_be_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1910, 0.6).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1930, 0.63).
narrative_ontology:measurement(qwer_be_t1940, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1940, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1880, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1880, 0.48).
narrative_ontology:measurement(qwer_su_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1890, 0.56).
narrative_ontology:measurement(qwer_su_t1900, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1900, 0.62).
narrative_ontology:measurement(qwer_su_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1910, 0.68).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1920, 0.71).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1930, 0.71).
narrative_ontology:measurement(qwer_su_t1940, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1940, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'why QWERTY persisted' decomposes into three structurally distinct constraints per the epsilon-invariance principle — naturalization_reading (fair-competition survival; negligible extraction, no parties beyond diffuse participants), lock_in_reading (path-dependent coordination failure; victims and switching costs present, agency diffused, no enforcement gate), and this beneficiary_extraction_reading (identifiable beneficiaries, active suppression, engineered switching costs; highest epsilon of the three). The upstream member is naturalization_reading (highest empirical confidence as a baseline claim; the other two are reactions to it — one refuting its fairness premise, one refuting its adequacy premise). Each sibling's network.affects_constraints links back to complete the family graph. The epsilon values differ because the referent differs: each reading assesses the SAME historical arrangement through its own lights, yielding different beneficiary structures, different enforcement facts, and different classifications. No single story hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
