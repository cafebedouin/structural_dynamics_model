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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Welfare-Regulated Animal Use (Sentience-Bounded Property Status)
 *   domain: moral philosophy/animal ethics/legal theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading — welfare_reading — of the contested
 *   animal_status_kernel: animals are sentient beings whose suffering is
 *   morally relevant; use is acceptable when regulated to minimize pain;
 *   property status is retained but bounded by welfare obligations. Per the
 *   epsilon-invariance principle the constraint is authored clean: the
 *   epsilon referent is the standing welfare-regulated animal-use arrangement
 *   as this reading itself assesses it — not the abolitionist arrangement it
 *   rejects and not the unregulated property arrangement it amends. The
 *   sibling readings (property_reading: considerability only via ownership,
 *   economic value exhausts value; abolitionist_reading: property status
 *   itself is the injustice, all use categorically impermissible) are
 *   separate constraints with separate epsilon values and victim sets, linked
 *   via network.affects_constraints. KEY AGENTS (by structural relationship):
 *   farmed_animals and laboratory_animals: primary targets
 *   (powerless/trapped) — bear continued use, confinement, and death under
 *   softened conditions; animal_use_industries: primary beneficiary with
 *   agenda-shaping reach (institutional/arbitrage) — retains use rights,
 *   shapes the rules it complies with, bears compliance costs;
 *   animal_product_consumers: beneficiary and partial payer
 *   (organized/constrained) — receive products and moral reassurance, pay
 *   passed-through costs; animal_welfare_organizations: identity-locked
 *   beneficiaries (organized/identity_locked) — collect institutional
 *   relevance from the reform project itself; welfare_regulators:
 *   agenda_setters (institutional/constrained) — write, inspect, and enforce;
 *   abolitionist_advocates: excluded voices (moderate/constrained) — reject
 *   the property premise and are outside the rooms where standards are
 *   written; animal_ethics_philosophers: analytical observers.
 *
 * KEY AGENTS:
 *   - farmed_animals: primary target (powerless/trapped) — lives and bodies remain available for human purposes birth to death under mandated treatment floors
 *   - laboratory_animals: primary target (powerless/trapped) — available for invasive procedures under protocol review that approves rather than prevents
 *   - animal_use_industries: primary beneficiary (institutional/arbitrage) — retains use rights, shapes standards, bears compliance costs, can relocate to lax jurisdictions
 *   - animal_product_consumers: beneficiary/payer (organized/constrained) — buy products and moral reassurance, absorb price pass-through
 *   - animal_welfare_organizations: identity-locked beneficiary (organized/identity_locked) — purpose and funding ride on the ongoing incremental-reform project
 *   - welfare_regulators: agenda_setter (institutional/constrained) — administer and enforce treatment standards
 *   - abolitionist_advocates: excluded voice (moderate/constrained) — argue property status is the injustice; absent from standard-setting
 *   - animal_ethics_philosophers: analytical observer (analytical/analytical) — supply the arguments the contending positions draw on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.58).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.6).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Welfare-Regulated Animal Use (Sentience-Bounded Property Status)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral philosophy/animal ethics/legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '8c953e71-2d76-4ad7-bdc6-d9079660eea0').
narrative_ontology:cs_kernel_codification('8c953e71-2d76-4ad7-bdc6-d9079660eea0', formalized).
narrative_ontology:cs_authority_grounding('8c953e71-2d76-4ad7-bdc6-d9079660eea0', lineage).
narrative_ontology:cs_interpretation_layer_present('8c953e71-2d76-4ad7-bdc6-d9079660eea0').
narrative_ontology:cs_reading_relation('8c953e71-2d76-4ad7-bdc6-d9079660eea0', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('8c953e71-2d76-4ad7-bdc6-d9079660eea0', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('8c953e71-2d76-4ad7-bdc6-d9079660eea0', foundational, sentience_grounds_moral_considerability).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_considerability, holdable).
narrative_ontology:cs_axiom_grounding('8c953e71-2d76-4ad7-bdc6-d9079660eea0', sentience_grounds_moral_considerability, deontological).
narrative_ontology:cs_axiom('8c953e71-2d76-4ad7-bdc6-d9079660eea0', foundational, use_permissible_when_suffering_minimized).
narrative_ontology:cs_axiom_status(use_permissible_when_suffering_minimized, holdable).
narrative_ontology:cs_axiom_grounding('8c953e71-2d76-4ad7-bdc6-d9079660eea0', use_permissible_when_suffering_minimized, instrumental).
narrative_ontology:cs_axiom('8c953e71-2d76-4ad7-bdc6-d9079660eea0', secondary, property_status_compatible_with_welfare_obligations).
narrative_ontology:cs_axiom_status(property_status_compatible_with_welfare_obligations, holdable).
narrative_ontology:cs_axiom_grounding('8c953e71-2d76-4ad7-bdc6-d9079660eea0', property_status_compatible_with_welfare_obligations, conventional).
narrative_ontology:cs_reference_frame('8c953e71-2d76-4ad7-bdc6-d9079660eea0', regulated_humane_use_within_property).
narrative_ontology:cs_drift_state('8c953e71-2d76-4ad7-bdc6-d9079660eea0', post_new_welfarism_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8c953e71-2d76-4ad7-bdc6-d9079660eea0', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_product_consumers).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, laboratory_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, animal_product_consumers).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, sentience_moral_relevance_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, welfare_reform_progressivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bred, housed, transported, and slaughtered under rules mandating space allowances, enrichment, stunning, and handling standards. The rules reduce some measurable suffering but do not alter that their lives and bodies remain available for human purposes from birth to death. They cannot refuse, leave, communicate, or advocate; every path open to them runs through the system that uses them.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Are used in research under protocols requiring justification of animal numbers, anesthesia, analgesia, and humane endpoints. Review committees weigh necessity and refinement but approve the overwhelming majority of applications; the animals' availability for invasive procedures is presupposed by the review structure itself, and they have no channel of refusal.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Raise, transport, process, and sell animals and animal products at scale. They fund and staff many standard-setting bodies, comply with welfare audits, and pass compliance costs into prices. When rules tighten in one jurisdiction they can shift production or sourcing to laxer ones, and they lobby continuously on the content of the rules themselves. The regulatory framework secures the social license their markets depend on.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_use_industries, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_use_industries, agenda_setter).

% Buy meat, dairy, eggs, leather, and other animal products, paying somewhat higher prices where welfare standards raise costs. Certification labels let them align purchases with moral concern without changing what they consume. Fully switching away from animal products is possible but runs against habit, price, cuisine, availability, and social practice.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_product_consumers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_product_consumers, payer).

% Campaign for stronger standards, run certification and outreach programs, hold seats on advisory bodies, and depend for purpose and funding on the ongoing project of improving treatment within continued use. Staff and donor identity are bound up with incremental reform; abandoning that approach would dissolve the organization's role, so the option is unavailable in practice however debatable it is in principle.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_welfare_organizations, beneficiary,
    organized, generational, identity_locked, global).

% Write, inspect, and enforce treatment standards through agriculture and research oversight agencies. They balance industry feasibility against welfare-science recommendations, rely heavily on industry-reported data for monitoring, and answer politically to both producer constituencies and concerned publics. Their careers and budgets are structured around administering the framework.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Argue that the property status of animals is itself the injustice and that welfare improvement entrenches it by reassuring the public. They publish, protest, support litigation, and run public education, but are largely absent from the standard-setting and advisory bodies where treatment rules are actually written, and several jurisdictions restrict their investigative and messaging tools by statute.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_advocates, excluded,
    moderate, civilizational, constrained, global).

% Analyze the status question from outside the regulatory process, producing the arguments on which the contending positions draw. They observe the structure of the debate, trace its lineages, and hold no enforcement, compliance, or standard-setting role.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_ethics_philosophers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__welfare_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_status_kernel__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets enforceable minimum treatment standards across dispersed producers, solving a race-to-the-bottom problem: absent common floors, competition drives treatment toward the cheapest tolerable level and consumers have no legible signal distinguishing practices. Shared standards also give the industries a stable social license and give welfare science a regulatory channel into practice.
% TRANSFER_FUNCTION: Moves compliance costs — space, enrichment, stunning equipment, slower processing lines, audit fees — onto industries and onward to consumers in prices; moves animals' bodies and products from animals to industries and consumers; moves moral reassurance and ethical-consumption standing to consumers; moves institutional relevance, funding, and advisory seats to welfare organizations and agencies.
% ABSENT_VOICES: The animals themselves — the heaviest payers under this reading's own lights — have no voice, vote, or representative with independent standing anywhere in standard-setting. Abolitionist advocates who reject the property premise are outside the advisory bodies. Communities bearing the environmental externalities of concentrated animal agriculture are rarely seated. Future people affected by land, water, and climate impacts are unrepresented.
% DISAPPEARANCE_RATIONALE: If welfare regulation vanished overnight, treatment would fall to the competitive floor within a few production cycles, the certification market and the organizations built on it would collapse, consumer trust signals would disappear, and the political contest over animal status would reorganize around raw prohibition-versus-unrestricted-use poles instead of standards-setting.
% FOUNDING_PROBLEM: Nineteenth-century anti-cruelty campaigns confronted visible gratuitous cruelty — working animals beaten, livestock slaughtered without stunning — with no legal floor on treatment and no remedy short of banning use altogether. The welfare settlement was built to make suffering-minimization enforceable while leaving use and property status intact.
% FOUNDING_PROBLEM_CORROBORATION: Veterinary ethology and animal-welfare science corroborate both that the founding problem was real (documented stress responses under unregulated practices) and that it persists at scale under current regulation (stereotypies and chronic-stress indicators in standard commercial housing). Abolitionist critics, testifying from outside every benefiting party, attest that suffering continues at enormous volume and dispute only whether regulation can ever suffice. Industry attests compliance, not the problem; no benefiting party's testimony is relied upon for the status call.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   The claimed_type tangled_rope follows from structure, not from the metrics: the arrangement possesses a genuine coordination function (enforceable treatment floors solving a race-to-the-bottom among dispersed producers, plus a legible consumer signal), declares both beneficiaries and victims, and requires active enforcement — the canonical tangled-rope triple. The metrics are authored descriptively and independently: epsilon 0.58 reflects moderate extraction under this reading's own lights — suffering is officially morally relevant, yet use and killing continue at scale, with compliance costs borne by industry and consumers partially offsetting the transfer; suppression 0.60 reflects legal foreclosure of the rights-based alternative (ag-gag statutes, labeling restrictions, exclusion of abolitionist voices from advisory bodies) compounded by public moral comfort — a structural/internalized mix flagged in omega; theater_ratio 0.42 reflects an assurance layer whose reassurance function increasingly outruns its verified condition-change function; accessibility_collapse 0.40 — the abolitionist legal alternative and plant-based consumption survive but the welfare frame absorbs much of the pressure they would otherwise generate; resistance 0.55 — industry lobbying against tightening, abolitionist critique, and recurring conflict over undercover investigation. All three series run on one shared seven-point grid (T0-T60, mapping to roughly 1965-2025: Brambell-era foundations through certification proliferation to the ag-gag era). suppression_requirement is tracked deliberately because the story specifically traces enforcement-capacity change: inspection build-out through the middle of the interval, then hardening against challenge (ag-gag and label-defense statutes) rather than decay. Extractiveness declines as standards diffuse, then creeps back up as total use expands faster than per-animal gains — the new-welfarism signature.
 *
 * PERSPECTIVAL GAP:
 *   From the industry seat the arrangement is a manageable compliance regime that purchases durable social license — cost-bearing but framework-preserving, with arbitrage available when any jurisdiction tightens. From the consumer seat it is moral reassurance at modest price: certification converts an unresolved moral question into a purchasable attribute. From the welfare-organization seat it is the institution's very purpose — staff and donor identity are fused with the incremental frame, so questioning whether regulation can ever suffice threatens the organization's existence (identity_locked exit; professional-and-institutional identity fusion rather than mere career path dependence). From the animals' position — computed from powerless and trapped with no coalition channel, since they have no voice, vote, or standing representative — the entire arrangement is simply the thing done to them: every other seat's benefit is funded by their continued availability for use and death. The regulator seat experiences the arrangement as a balancing act it administers rather than a structure it chose. The engine computes these divergent per-seat types from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: farmed_animals and laboratory_animals are declared victims with powerless power and trapped exit — pinned at the full-target end, amplified by absolute exit absence; no coalition correction is possible because the class has no communication, organization, or franchise channel at all. animal_use_industries are declared beneficiaries, but the derivation would push them to near-full-beneficiary on role alone, ignoring that they bear real compliance costs and consistently prefer weaker rules — hence the institutional override to 0.28 (net beneficiary, materially cost-bearing). animal_product_consumers are declared beneficiaries with a payer secondary role; the derivation would read the beneficiary declaration strongly, so the organized override to 0.32 records the passed-through costs and residual moral discomfort that place them nearer symmetric than the role implies. animal_welfare_organizations keep the derived near-beneficiary value: their benefit is real and their identity lock deepens investment in the arrangement's persistence rather than offsetting it. Regulators sit mid-range as administrators. Suppression is authored as a raw structural property and is NOT scaled by power or scope in the engine's computation; only extractiveness is scaled, by directionality and spatial scope — the global scope of industrial animal agriculture modestly amplifies effective extraction on the target seats by making verification harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The welfare settlement invites two opposite mislabels, and the tangled_rope claim is what keeps both honest. Read as its proponents present it — a progressive solution steadily reducing harm en route to something better — it masquerades as a rope or scaffold; read as its abolitionist critics present it — moral licensing that entrenches property status by making the public comfortable — it masquerades as a snare whose coordination story is cover. The structural truth contains both: the coordination function is genuine (treatment indicators measurably improve relative to the unregulated counterfactual) and the extraction is real (animals remain usable and killable inside the same structure that softens their treatment). Mandatrophy is NOT resolved: the founding problem — enforceable suffering-minimization within continuing use — is live, the mandate still describes ongoing activity, and no sunset clause exists. The danger signal is temporal: theater_ratio rising monotonically while total use expands suggests the mandate is drifting toward performed concern rather than reduced suffering. If that drift completes — assurance activity fully decoupled from condition-change — the label layer decays toward piton while the use relation underneath hardens toward snare; the measurement series is designed to expose exactly that fork.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (welfare_reading) of the animal_status_kernel; how would instantiating a sibling reading instead change the structure?',
    'Comparative generation of the sibling files (property_reading, abolitionist_reading) and cross-reading audit of victim-set boundaries, epsilon values, and per-seat classifications over the same factual substrate.',
    'Under the property_reading the victim-set empties (no independent moral considerability) and epsilon collapses toward zero by construction; under the abolitionist_reading the victim-set becomes categorical (property status itself is the injustice) and epsilon over the use relation goes maximal. The welfare reading''s moderate epsilon and partial victim-set exist only relative to this reading''s sentience-bounded frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which kernel, which reading, where the sibling disagreement is located.').

omega_variable(
    welfarism_moral_licensing,
    'Does welfare reform reduce net suffering, or does the moral reassurance it generates (''happy meat'' comfort) increase total animal use enough to outweigh per-animal gains?',
    'Consumption and production data in jurisdictions before and after prominent welfare reforms, against matched controls; elasticity of demand for certified products; longitudinal totals of animals used.',
    'If moral licensing dominates, the arrangement''s net effect on animals worsens despite rising standards, effective extraction exceeds the authored epsilon, and the payer-seat classification drifts toward the pure-extraction end; if per-animal gains dominate, the coordination-function reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfarism_moral_licensing, empirical, 'Whether the new-welfarism critique identifies a real net-harm mechanism.').

omega_variable(
    certification_theater_share,
    'What fraction of certified-welfare assurance activity corresponds to materially different conditions for the animals, versus label-level reassurance over substantially unchanged housing and handling?',
    'Paired audit studies comparing certified and conventional facilities on behavioral and physiological welfare indicators (stereotypy rates, stress markers, space utilization), controlling for scheme tier.',
    'A high unverifiable share means the theater_ratio is understated and the label layer is decaying toward inertial performance; a low share means the certification layer performs real condition-change and the coordination function is stronger than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_theater_share, empirical, 'Verified-versus-performed split inside the welfare assurance layer.').

omega_variable(
    suppression_source_ambiguity,
    'Is the marginalization of the abolitionist alternative structural (legal foreclosure: ag-gag statutes, labeling restrictions, exclusion from standard-setting bodies) or internalized (public moral comfort with regulated use dissolving demand for the alternative)?',
    'Post-legislative trajectory tracking: if abolitionist pressure revives where structural barriers are removed (litigation wins, disclosure protections) but stalls where public satisfaction is high despite open channels, the internalized component dominates; the reverse split attributes suppression to structure.',
    'If mostly structural, removal of specific statutes would reopen the alternative space and the constraint''s persistence depends on active defense; if mostly internalized, the suppression travels in public attitudes and outlives any particular statute, making the arrangement harder to dislodge than its legal surface suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, conceptual, 'Structural versus internalized mix in the suppression of alternatives to regulated use.').

omega_variable(
    cs_authority_framing_underdetermination,
    'Is the welfare apparatus''s authority grounded in lineage (continuity with the Benthamite reform tradition, with welfare science as interpretive buffer) or in extraction (an industry-regulator complex whose authority depends on keeping the contest at the level of treatment standards rather than status)?',
    'Trace adjudication history: if standard revisions consistently track welfare-science findings regardless of industry position, the lineage/expertise framing holds; if revisions systematically stall or reverse where status questions would be opened, the extraction framing fits.',
    'Under the lineage framing the commitment system absorbs drift through its interpretive layer and the reading is internally stable; under the extraction framing the authority structure is a captured variant and the cs_pattern classification shifts toward extraction-grounded, feeding the snare-side per-seat reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_underdetermination, conceptual, 'Two coherent framings of the same authority structure yield different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__welfare_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__welfare_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__welfare_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__welfare_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement_basis(anim_tr_t40, observed).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__welfare_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(anim_tr_t50, observed).
narrative_ontology:measurement(anim_tr_t60, animal_status_kernel__welfare_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(anim_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__welfare_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__welfare_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__welfare_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__welfare_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(anim_be_t40, observed).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__welfare_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement_basis(anim_be_t50, observed).
narrative_ontology:measurement(anim_be_t60, animal_status_kernel__welfare_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(anim_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__welfare_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__welfare_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__welfare_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__welfare_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(anim_su_t40, observed).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__welfare_reading, suppression_requirement, 50, 0.59).
narrative_ontology:measurement_basis(anim_su_t50, observed).
narrative_ontology:measurement(anim_su_t60, animal_status_kernel__welfare_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(anim_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'animal status' names one debate but decomposes, per the epsilon-invariance principle, into three structurally distinct constraints: the property reading (no independent victim-set; epsilon collapses toward zero from the animals' side by construction), the welfare reading (partial victim-set via sentience; moderate epsilon over the standing regulated-use arrangement), and the abolitionist reading (categorical victim-set; maximal epsilon over the use relation). Each reading gets its own file, its own epsilon, its own beneficiaries and victims; whichever reading a jurisdiction adopts structurally reshapes the operating environment of the others (adoption of abolition forecloses the property reading's premises in law; entrenchment of welfare raises the mobilization cost of abolition). This file links both siblings; the family is complete only when all three stories exist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, institutional, 0.28).
constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
