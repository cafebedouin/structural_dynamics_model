% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Welfare Reading of Animal Status: Sentient Beings as Constrained Property Under Regulated Use
 *   domain: moral philosophy/animal ethics/legal theory
 *
 * SUMMARY:
 *   The standing arrangement this story is about — assessed by this reading's
 *   own lights — is the regime in which animals are legally property while
 *   their sentient suffering counts directly: use of animals is permitted and
 *   lawful subject to enforceable welfare obligations (housing minimums,
 *   stunning requirements, transport rules, inspection and labeling regimes).
 *   The welfare reading supplies the operative law of every developed
 *   jurisdiction: it retains the property category, grafts obligations onto
 *   it, and channels the ethics of animal use into the question of how much
 *   suffering use may involve. Epsilon's referent is this standing
 *   arrangement, not the abolitionist alternative this reading declines to
 *   endorse: by its own lights the arrangement burdens animals substantially
 *   (they remain confined, bred, transported, and killed) while imposing
 *   real, though survivable, costs on industry. This file is one member of a
 *   three-story constraint family decomposing the colloquial label 'animal
 *   status'; the property and abolitionist readings are separate stories
 *   linked through network.affects_constraints. Claimed type and metrics are
 *   authored independently: the type from structural analysis, the metrics
 *   from descriptive operation.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) — bear the arrangement's burdens in full; their interests reach every table only through proxies
 *   - animal_agriculture_industry: primary beneficiary with cost-bearing counterweight (institutional/arbitrage) — collects revenue and legitimacy, absorbs and passes compliance costs
 *   - animal_product_consumers: near-symmetric seat (moderate/mobile) — receive certified products and moral reassurance, pay compliance premia
 *   - welfare_regulatory_bodies: agenda-setter (institutional/identity_locked) — administers the standards; institutional identity fused with the welfare project
 *   - welfare_advocacy_organizations: frame-dependent beneficiary (organized/identity_locked) — the arrangement's loudest critic and one of its structural dependents
 *   - smallholder_farmers: secondary target (moderate/constrained) — bear disproportionate fixed compliance costs with the least absorption capacity
 *   - abolitionist_theorists_and_activists: excluded voice (organized/mobile) — deny the frame's permissibility premise; outside the standard-setting conversation
 *   - veterinary_and_animal_science_establishment: observer with an agenda-setting hand (institutional/analytical) — operationalizes 'suffering' and certifies compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.58).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.55).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Welfare Reading of Animal Status: Sentient Beings as Constrained Property Under Regulated Use").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral philosophy/animal ethics/legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, 'efdc0c20-33e5-47c0-8aca-fb9bcca93f10').
narrative_ontology:cs_kernel_codification('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', distributed).
narrative_ontology:cs_authority_grounding('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', distributed).
narrative_ontology:cs_reading_relation('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', foundational, sentience_grounds_direct_moral_considerability).
narrative_ontology:cs_axiom_status(sentience_grounds_direct_moral_considerability, holdable).
narrative_ontology:cs_axiom_grounding('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', sentience_grounds_direct_moral_considerability, empirically_contingent).
narrative_ontology:cs_axiom('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', foundational, regulated_use_permissible_under_welfare_constraints).
narrative_ontology:cs_axiom_status(regulated_use_permissible_under_welfare_constraints, holdable).
narrative_ontology:cs_axiom_grounding('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', regulated_use_permissible_under_welfare_constraints, instrumental).
narrative_ontology:cs_reference_frame('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', sentient_property_with_welfare_obligations).
narrative_ontology:cs_drift_state('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('efdc0c20-33e5-47c0-8aca-fb9bcca93f10', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_product_consumers).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, welfare_regulatory_bodies).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, welfare_advocacy_organizations).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, smallholder_farmers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, animal_product_consumers).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, sentience_threshold_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, regulability_of_instrumental_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bred, confined, transported, and slaughtered at scale under standards that specify space allowances, anesthesia requirements, and handling rules. What flows from them: bodies, labor, milk, eggs, offspring. What flows to them: conditions calibrated to the governing standard's thresholds, which reduce but do not eliminate aversive experience. No exit exists in any ordinary sense — the standards govern the conditions of their use, not whether use occurs — and their interests reach the standard-setting table only through human representatives.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).

% Produces animal products under welfare statutes that impose housing, transport, and slaughter requirements, inspection regimes, and record-keeping duties. Collects the revenue from what animal use yields. Bears compliance costs, which large integrated firms absorb and pass through more easily than small ones, and benefits from the legitimacy that certified compliance confers with consumers and export markets. Can shift production between jurisdictions with different standards, substitute species or automation, or pass costs to buyers; the enterprise itself has no reason to leave the arrangement.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_agriculture_industry, payer).

% Buy animal products at prices that include a compliance premium and receive in exchange a supply chain certified as humane, which licenses continued purchase without personal investigation. Individually weak; collectively their demand sets the market. Exit — shifting to plant-based alternatives — is available and increasingly cheap, but taken unevenly, since habit, taste, and price bind most purchases. Also carries the arrangement's deferred moral exposure, which the certification is designed to quiet.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_product_consumers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_product_consumers, payer).

% Administer the welfare statutes: write implementing rules, fund inspections, prosecute violations, certify labels. Their mandate, budget, and staff expertise exist because the arrangement exists, and their public identity is bound to being the guardian of humane treatment. Tightening standards wins credit with advocacy constituencies; loosening wins industry cooperation; acknowledging that the framework's own aims go unmet by design would undercut the office's justification.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_regulatory_bodies, agenda_setter,
    institutional, generational, identity_locked, national).

% Campaign for stronger welfare standards, publish investigations, litigate, and fundraise on the gap between actual conditions and the public's image of them. Their donor base, media access, and institutional partnerships are built inside the welfare frame; a pivot to opposing use altogether would break those relationships. They are the arrangement's loudest critics and among its structural dependents.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, global).

% Operate at scales where fixed compliance costs — retrofitted housing, paperwork, inspection fees — consume a disproportionate share of margin. Many entered the trade before current standards and financed premises that now require costly conversion. Exiting means liquidating herds and equipment and losing a livelihood tied to place and family history; converting to plant production demands new capital and skills. They bear the arrangement's costs with the least capacity to absorb or pass them.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, smallholder_farmers, payer,
    moderate, biographical, constrained, regional).

% Argue that the wrong is the property relation itself, not the conditions inside it, and organize for ending use rather than reforming it. They sit outside the standard-setting consultations, which are structured around how much suffering use may involve, not whether it may occur. Their critique of welfare reform as public pacification is published, taught, and largely unrepresented at the tables where standards are written.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_theorists_and_activists, excluded,
    organized, generational, mobile, global).

% Supplies the operational content of welfare standards: pain indices, stocking-density limits, stunning requirements, audit protocols. Universities and professional bodies certify inspectors and publish the welfare science the standards cite. Holds a dual position: its findings repeatedly extend the circle of plausibly sentient candidates and tighten what the science supports, while its consulting arm is paid by the industry it assesses.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, veterinary_and_animal_science_establishment, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, veterinary_and_animal_science_establishment, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status_kernel__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine multi-party problem: absent common welfare floors, competitive pressure drives every producer toward the cheapest handling regardless of suffering — a race to the bottom no unilateral actor can exit profitably. The standards also coordinate public trust (a single legible certification lets buyers purchase animal products without personal investigation) and standardize inspection, labeling, and veterinary practice across producers and jurisdictions.
% TRANSFER_FUNCTION: Moves animal bodies, labor, milk, eggs, and reproductive output from farmed animals to producers and consumers; moves compliance costs from the public purse to producers (partly passed to buyers); moves moral reassurance from the regulatory apparatus to consumers; and moves information about on-farm conditions toward the public in the amounts the enforcement and disclosure rules permit.
% ABSENT_VOICES: The animals whose treatment is being standardized cannot be present; their interests arrive only through proxies — advocacy organizations whose institutional survival depends on the frame they criticize, and scientists whose consulting income ties them to industry. Abolitionist voices are excluded from standard-setting consultations by the consultations' own terms, which admit only the question of how much suffering use may involve. Both absences are structural, not incidental.
% DISAPPEARANCE_RATIONALE: Overnight repeal returns animals to unconstrained property: confinement densities, transport durations, and slaughter practices revert to the cost-minimizing frontier within a few production cycles; prices dip then stabilize; the certification and labeling economy collapses, taking a chunk of retail trust with it; advocacy organizations refound around re-legislation; export markets re-sort as trading partners impose their own standards at the border. Food supply as such does not depend on the arrangement — plant-based and cultivated alternatives exist — but the entire animal-use economy reorganizes around its absence.
% FOUNDING_PROBLEM: Reconcile the industrial expansion of animal use with the recognized fact of animal suffering: prevent the worst treatment while preserving use. The bargain was struck when nineteenth-century anti-cruelty law met twentieth-century intensive farming (the Brambell moment) and is renewed each time a new domain — broilers, fish, aquaculture, insects — enters the use economy.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the independent animal-welfare science literature attests both the reality of the suffering and the persistent gap between codified standards and on-farm conditions; abolitionist scholarship attests the problem is real while denying this frame can close it; the historical record of the Brambell Committee hearings documents the founding bargain's terms. Industry attests the problem is 'managed,' which is a different claim; no participant attests it is solved.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.58: by this reading's own standard — suffering is directly morally relevant — the standing arrangement imposes severe burdens on tens of billions of animals annually while regulation trims rather than removes them; the counterweight is the real cost the arrangement forces onto industry, which is why epsilon sits mid-range rather than high. Suppression 0.55 is authored as a raw structural property, unscaled by power or scope: ag-gag statutes, controlled inspection access, and information asymmetry are structural mechanisms; consumer self-comfort and advocacy frame-lock are internalized ones — roughly half each, per the dedicated omega. Theater 0.45: welfare gains are real where enforcement bites, but a growing share of activity is label economics — certification schemes whose thresholds sit far below the public's imagined barnyard. Accessibility collapse 0.4: abolitionist and plant-based alternatives remain visibly live; the frame channels reform into itself without closing exits. Resistance 0.55: industry fights tightening through lobbying and exemption carve-outs; abolitionists fight the frame itself. Seat divergence: the farmed_animals seat (powerless, trapped, full-target directionality) computes a heavily extractive experience of the same structure from which the industry seat (beneficiary, arbitrage exit) computes manageable overhead — the engine derives this from the declarations, not from the claim. Coalition note: farmed animals cannot form coalitions; their interests arrive only through proxies whose incentives the advocacy seat illustrates, while smallholder coalitions are possible but chronically fragmented. The measurement series share one grid (T in years since 1965, the Brambell anchor): burden dips as the first binding reforms land (T=10-20), then climbs as intensification and aquaculture outrun coverage; theater climbs monotonically with the labeling economy; suppression climbs as transparency itself is policed.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute different types from one structure. From the industry seat the arrangement is a manageable compliance surface that purchases legitimacy and export access; from the farmed_animals seat it is the totality of a life, regulated. The sharpest divergence is the advocacy seat: nominally the animals' champion, structurally a frame-dependent beneficiary — its funding, access, and identity require the welfare contest to continue inside welfarist terms, which is precisely the 'new welfarism' critique the abolitionist seat presses. Same-power divergence: consumers and smallholder farmers both sit at moderate power, yet exit differs radically — consumers can switch purchases weekly, smallholders face liquidation of herds, premises, and place-bound family livelihoods — so identical nominal standing yields opposite directionalities. Inter-institutionally, regulators experience capture pressure from the industry seat while drawing legitimacy from the advocacy seat, and the veterinary establishment operationalizes 'suffering' for both while being paid by one of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. farmed_animals: victims, powerless, trapped — nearest the full-target end; no arbitrage exists at any price. smallholder_farmers: victims, moderate, constrained — high target-side weighting, softened only by partial cost pass-through. animal_agriculture_industry: beneficiary with a secondary payer position and arbitrage exit — near the beneficiary end; compliance costs are real but recovered through pricing and legitimation rents. animal_product_consumers: beneficiary with a secondary payer position and mobile exit — near-symmetric, tilted beneficiary: certified reassurance received, compliance premium paid. welfare_regulatory_bodies and welfare_advocacy_organizations: beneficiaries whose identity_locked exits fuse them to the frame's continuation. The veterinary establishment observes and operationalizes rather than collecting. Scope amplification: the arrangement operates globally, where verification is hardest — effective burden concentrates on the seats least able to verify conditions or appeal outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconcile expanding use with recognized suffering — is live, so no mandatrophy is declared: the arrangement's mandate still binds its operation. The risk runs the other direction: the theater series shows the mandate slowly converting into performance (label economics displacing enforcement substance), the classic precursor of a mandate outliving its function while the underlying activity continues. If the theater trend crossed dominance, the arrangement would persist as ritual atop unchanged practice — at which point the mismatch between founding-problem status and persistence would flag. The measurements exist to date that crossing if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading — welfare_reading — of the contested kernel animal_status_kernel; what would the sibling readings (property_reading, abolitionist_reading) change structurally, and where exactly does the disagreement sit?',
    'No dataset resolves a conceptual contest; resolution arrives only if a governing institution adopts one reading wholesale, at which point the sibling story''s victim set and epsilon replace this one''s.',
    'Under property_reading the victim set empties (considerability runs through ownership and economic value only) and the arrangement''s burdens vanish by definitional fiat; under abolitionist_reading the victim set expands to the entire use-relation, regulated use becomes impermissible, and the arrangement loses its coordination defense entirely. The disagreement is located at the moral-status axiom: what grounds considerability — economic value only, sentience, or personhood.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the animal-status kernel; siblings would redraw the victim set and the permissibility scope.').

omega_variable(
    victim_set_boundary_ambiguity,
    'Which taxa fall inside the victim set via suffering-capacity — vertebrates only, or also cephalopods, crustaceans, decapods, fish, insects?',
    'Comparative nociception and cognition research programs; recent statutory recognitions of crustacean and cephalopod sentience show the boundary already moving.',
    'Each outward extension grows the population bearing burdens faster than standards extend coverage, raising effective burden and pushing classification toward the extractive pole; contraction does the reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_ambiguity, empirical, 'The victim-set boundary is empirically open and migrates with sentience science.').

omega_variable(
    new_welfarism_legitimation_effect,
    'Does welfare reform reduce net suffering, or does certified humaneness expand total use by comforting consumers into continued purchase (the ''happy meat'' effect pressed by abolitionist critics)?',
    'Econometric study of reform episodes: total production and consumption before and after major welfare mandates, controlling for price and income effects.',
    'If legitimation dominates, the coordination function partly services the very use it moderates and classification trends toward the extractive pole; if restraint dominates, the coordination reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(new_welfarism_legitimation_effect, empirical, 'Whether the arrangement''s legitimating function nets against or compounds its moderating function.').

omega_variable(
    suppression_mechanism_split,
    'How much of the measured suppression is structural (ag-gag statutes, controlled inspection access, information asymmetry) versus internalized (consumer self-comfort, advocacy frame lock-in, normalized purchase habits)?',
    'Natural experiments: repeal of ag-gag laws followed by investigation-rate trajectories; post-revelation consumption persistence studies.',
    'If the internalized share is large, suppression persists after structural barriers fall — the arrangement''s stabilizing force exceeds what its statutes show, and transparency-led reform underdelivers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, conceptual, 'Structural versus internalized composition of the suppression scalar.').

omega_variable(
    enforcement_gap_measurement,
    'How wide is the gap between codified welfare standards and conditions on actual operations — and does announced auditing measure compliance or performance?',
    'Randomized unannounced third-party audits benchmarked against announced industry audits and undercover investigation findings.',
    'A wide gap raises effective burden and the performative share of enforcement activity, dating any drift toward inertial or purely extractive operation earlier than announced data would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_measurement, empirical, 'Codified-standard versus realized-condition divergence.').

omega_variable(
    cs_framing_under_determination,
    'Is the welfare arrangement''s authority structure best read as distributed (no single adjudicator; legislatures, agencies, science bodies, and industry boards produce competing determinations) or as extraction-grounded (standard-setting consultations in which the regulated industry holds the pen, making stability of the welfare settlement the source of institutional benefit)?',
    'Process tracing of standard-setting episodes: who drafts, whose comments survive, reversal rates of industry-favored provisions.',
    'Under the extraction framing, the arrangement''s authority rests on preventing revision of the welfare settlement and its classification drifts toward the extractive pole; under the distributed framing it remains a contested coordination device.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Two coherent framings of the same authority structure yield different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__welfare_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__welfare_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__welfare_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__welfare_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(anim_tr_t40, observed).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__welfare_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(anim_tr_t50, observed).
narrative_ontology:measurement(anim_tr_t60, animal_status_kernel__welfare_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement_basis(anim_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__welfare_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__welfare_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__welfare_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__welfare_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(anim_be_t40, observed).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__welfare_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(anim_be_t50, observed).
narrative_ontology:measurement(anim_be_t60, animal_status_kernel__welfare_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(anim_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__welfare_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__welfare_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__welfare_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__welfare_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement_basis(anim_su_t40, observed).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__welfare_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement_basis(anim_su_t50, observed).
narrative_ontology:measurement(anim_su_t60, animal_status_kernel__welfare_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement_basis(anim_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'animal status' per the epsilon-invariance principle: the label covers three structurally distinct constraints differing in victim-set membership and permissibility scope. property_reading (no animal victims; considerability via ownership), welfare_reading (this file; partial victim inclusion via sentience; use permitted under constraint), abolitionist_reading (full victim inclusion; all use impermissible). Each story carries its own epsilon, beneficiaries, and victims; the welfare reading sits genealogically between the other two — it amends the property category from within while absorbing abolitionist pressure from without — so this file links both siblings. Measuring 'animal ethics' with a single observable would average incompatible epsilons; the family structure prevents that.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
