% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: Article 3 Positive Entitlement: State Obligation to Provide Material Conditions for Life and Security
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights states: 'Everyone
 *   has the right to life, liberty and security of person.' This constraint
 *   story instantiates the POSITIVE ENTITLEMENT READING: Article 3 obligates
 *   states to actively provide the material conditions (welfare, healthcare,
 *   housing, security) necessary for life and dignity. Under this reading,
 *   subsistence is a constitutional right enforceable against the state;
 *   poverty and preventable disease are violations of Article 3. The reading
 *   creates justiciable claims that vulnerable populations can bring against
 *   states, forcing budget allocation and wealth redistribution. The
 *   competing sibling readings—negative liberty (Article 3 merely prohibits
 *   state killing/torture) and procedural hybrid (Article 3 guarantees due
 *   process)—are NOT described here; they are separate constraint stories.
 *   This story is one coherent ε-invariant account of the positive
 *   entitlement reading's logic and structure. The claim/metric intentional
 *   gap: this reading is CLAIMED as tangled_rope (genuine coordination
 *   function + asymmetric extraction via redistribution) while the metrics
 *   describe substantial extractiveness (0.68), moderate suppression of
 *   expression (0.42), and rising enforcement intensity over the interval.
 *   The engine computes seat-specific types from this structural data; the
 *   claim and metrics remain independent.
 *
 * KEY AGENTS:
 *   - vulnerable_populations: powerless, trapped exit, beneficiary of welfare provision and healthcare access
 *   - state_enforcement_apparatus: institutional power, controls Article 3 interpretation and budget allocation
 *   - property_rights_holders: powerful, constrained exit, targeted for wealth extraction via progressive taxation and regulatory takings
 *   - high_income_earners: powerful to mobile, subject to progressive taxation justified by Article 3 obligations
 *   - expression_constrained_actors: moderate power, constrained exit, suppressed by hate-speech laws and forced-association mandates
 *   - negative_liberty_advocates: organized but excluded, their core premise (state should abstain) is incommensurable with positive entitlement logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.42).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "Article 3 Positive Entitlement: State Obligation to Provide Material Conditions for Life and Security").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '4c88bf09-76d8-45d7-aff4-805578a66b3c').
narrative_ontology:cs_kernel_codification('4c88bf09-76d8-45d7-aff4-805578a66b3c', fixed_text).
narrative_ontology:cs_authority_grounding('4c88bf09-76d8-45d7-aff4-805578a66b3c', lineage).
narrative_ontology:cs_interpretation_layer_present('4c88bf09-76d8-45d7-aff4-805578a66b3c').
narrative_ontology:cs_reading_relation('4c88bf09-76d8-45d7-aff4-805578a66b3c', udhr_article_3__negative_liberty_reading, forecloses).
narrative_ontology:cs_reading_relation('4c88bf09-76d8-45d7-aff4-805578a66b3c', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('4c88bf09-76d8-45d7-aff4-805578a66b3c', foundational, right_to_life_requires_material_provision).
narrative_ontology:cs_axiom_status(right_to_life_requires_material_provision, holdable).
narrative_ontology:cs_axiom_grounding('4c88bf09-76d8-45d7-aff4-805578a66b3c', right_to_life_requires_material_provision, deontological).
narrative_ontology:cs_axiom('4c88bf09-76d8-45d7-aff4-805578a66b3c', foundational, state_affirmative_duty_to_redistribute).
narrative_ontology:cs_axiom_status(state_affirmative_duty_to_redistribute, holdable).
narrative_ontology:cs_axiom_grounding('4c88bf09-76d8-45d7-aff4-805578a66b3c', state_affirmative_duty_to_redistribute, instrumental).
narrative_ontology:cs_reference_frame('4c88bf09-76d8-45d7-aff4-805578a66b3c', post_wwii_human_dignity_foundation).
narrative_ontology:cs_drift_state('4c88bf09-76d8-45d7-aff4-805578a66b3c', contemporary_neoliberal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c88bf09-76d8-45d7-aff4-805578a66b3c', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, subsistence_access_claimants).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_rights_holders).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, high_income_earners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, expression_constrained_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and families lacking access to food, healthcare, adequate housing, or basic security. Under the positive entitlement reading, Article 3 creates a legal claim on the state for material provision of these conditions. Their exit options are virtually zero: they cannot opt out of the state or purchase survival elsewhere without resources. The reading makes their material insecurity a violation of fundamental rights rather than a charity question or market outcome.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Courts, legislatures, executive agencies, and fiscal authorities that must interpret and implement Article 3 as an affirmative obligation. This reading creates justiciable claims that can force budget allocation, regulatory intervention, and wealth transfer mechanisms. The apparatus bears the institutional responsibility for translating the entitlement into material provision.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and corporations whose property rights are constrained by wealth redistribution, progressive taxation, mandatory welfare contributions, and regulatory takings justified by Article 3 entitlements. They bear the extraction via reduced asset security, higher tax burden, and mandatory service provision. Their exit options are migration (capital flight, relocation to low-redistribution jurisdictions) or legal challenge—both costly and incomplete.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_rights_holders, payer,
    powerful, biographical, constrained, national).

% Individuals whose income is subject to progressive taxation and mandatory welfare contributions justified by the state's Article 3 obligation to provide material conditions. They experience the extraction as a claim on their future earnings. Mobile earners can exit partially via relocation; trapped earners face the full extraction.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, high_income_earners, payer,
    powerful, biographical, mobile, national).

% Individuals and organizations whose speech, association, or expressive conduct is restricted by hate-speech laws, forced-association mandates, or regulatory controls justified by Article 3's obligation to protect vulnerable populations' security and dignity. They bear the suppression cost directly: reduced speech space, increased legal liability, enforced participation in redistributive schemes.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, expression_constrained_actors, payer,
    moderate, biographical, constrained, national).

% Constitutional scholars, legislators, and judges who argue Article 3 creates only negative rights (freedom from state violence) rather than positive entitlements. They are structurally excluded from the positive entitlement reading's framework—their core premise (state obligation is to abstain, not provide) is incompatible with this reading's operative logic. They would object that Article 3 is being rewritten to impose unlimited affirmative obligations.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, negative_liberty_advocates, excluded,
    organized, generational, constrained, national).

% Constitutional courts and international human rights bodies that must decide whether Article 3 generates justiciable positive entitlements or merely aspirational guidance. Their rulings either activate the constraint (entitlements enforceable, states must provide) or neutralize it (entitlements non-justiciable, political branches decide). They observe the contest but their decisions constitute the constraint itself.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, judicial_review_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, vulnerable_populations).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective provision of material conditions (food, healthcare, housing, security) necessary for survival and dignity, solving market failure where individual purchasing power is insufficient. Internalizes health and security externalities (communicable disease, crime driven by subsistence deprivation) by pooling resources for universal provision. Solves the collective-action problem that vulnerable populations cannot generate demand through market prices (they have no income to express demand with) and thus markets undersupply life-sustaining goods to this population.
% TRANSFER_FUNCTION: Transfers wealth and property rights from high-income earners and property holders to vulnerable populations via: (1) progressive taxation, (2) mandatory welfare contributions, (3) regulatory takings (housing controls, rent limits), (4) required service provision (healthcare, education). Also restricts expressive conduct (hate speech, discriminatory speech) judged threatening to vulnerable populations' security and dignity. The net direction is downward (from powerful to powerless) and enforceable via state coercion (tax collection, regulatory enforcement, police suppression of prohibited speech).
% ABSENT_VOICES: Negative-liberty advocates (constitutional scholars, libertarian legislators, property-rights organizations) are structurally excluded from the positive entitlement reading's framework. They would argue Article 3 mandates only state abstention from deprivation, not affirmative provision—and that redistribution violates Article 3's protection of liberty and property. Their objections appear in courts and legislatures but their core premise (state obligation is to abstain) is incommensurable with the positive entitlement logic (state obligation is to provide). Also partially excluded: poor-state governments who argue that Article 3 positive entitlements are unachievable given fiscal constraints and thus represent an impossible mandate.
% DISAPPEARANCE_RATIONALE: If Article 3 disappeared as a positive entitlement, states would no longer face justiciable claims to provide welfare, healthcare, housing, or material security. Vulnerable populations would lose the legal right to subsistence and would depend on legislative discretion, voluntary charity, or market access for survival. Property rights would be more secure against redistribution claims. Progressive taxation would lose constitutional grounding and could be repealed. Hate-speech restrictions justified by vulnerable populations' Article 3 security would be legally vulnerable. The material and political organization of welfare states would be fundamentally reorganized—toward either market-based provision, explicit legislative welfare programs without constitutional mandate, or reduced provision. The world's social safety net would be reordered.
% FOUNDING_PROBLEM: Article 3 was drafted in the aftermath of World War II and the Holocaust, when the international community recognized that subsistence deprivation and economic insecurity were drivers of genocide, totalitarianism, and mass atrocity. The founding problem was: How do we ensure every human being has access to the material conditions (food, shelter, healthcare, security) necessary for life and dignity, not merely legal freedom from state torture? The Nuremberg tribunals and Holocaust documentation showed that mass killing depended on prior dispossession, hunger, and dehumanization. The positive entitlement reading answers: states have affirmative obligations to provide these conditions; subsistence is a fundamental right enforceable against the state.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from OUTSIDE the benefiting parties: (1) Independent historical analysis (Humphrey, Cassin records, Cold War documentation) showing the founding problem was genuinely alive in 1945: subsistence deprivation and genocide were perceived as linked threats. (2) Public health data documenting ongoing preventable mortality and morbidity linked to poverty in contemporary high-inequality states, supporting the continued relevance of the founding problem. (3) Economic analysis (Sen, Nussbaum, Rawls, independent scholars) arguing that market provision alone does not reach subsistence-level access for vulnerable populations. (4) HOWEVER, counter-corroboration from property-rights advocates and fiscal-conservative economists argues the founding problem is substantially solved (subsistence is not the primary driver of contemporary atrocity; totalitarianism arises from state power regardless of welfare provision) and Article 3 positive entitlements are misdirected solutions. The founding problem's status remains genuinely contested: vulnerable-population advocates argue it is live; negative-liberty and market-efficiency advocates argue it is dead or misidentified.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The positive entitlement reading creates a TANGLED_ROPE structure: (1) Genuine coordination function: states pooling resources to ensure subsistence access solves a real collective-action problem (individual market provision leaves vulnerable populations below survival threshold; public provision internalizes health and security externalities). (2) Asymmetric extraction: wealth and property rights holders are targeted for redistribution; the extraction is asymmetric because powerful actors pay while vulnerable actors receive. (3) Active enforcement: the constraint's persistence requires courts to interpret Article 3 as justiciable, legislatures to allocate budgets, tax authorities to collect, and police/regulatory bodies to suppress defection (property rights claims, anti-redistribution speech, tax evasion). Extractiveness rises from 0.42 to 0.68 over the 80-year interval as states progressively enforce the reading (welfare-state expansion, healthcare mandates, housing regulations, hate-speech law). Theater remains moderate (0.28) because genuine provision machinery exists alongside extractive enforcement—the care-provision function is real, not purely performative. Suppression is moderate (0.42) and stable because the constraint suppresses two different things: (a) property-rights claimants' legal arguments (constitutional takings doctrine constrained), and (b) expressive conduct judged threatening to vulnerable populations' security (hate speech, discriminatory speech). Neither suppression is total, hence moderate rather than high. The measurement series show extraction and suppression rising sharply for ~50 years (welfare-state buildout post-WWII), then flattening (state provision stabilizes at a policy equilibrium; further expansion faces political limits). This plateau is not complacency—it reflects the reading's maturation into institutional architecture.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence between payer and beneficiary is structural and will be computed by the engine from the authored data. From the property_rights_holders' seat: Article 3 positive entitlement is an illegitimate reinterpretation that expands state power and imposes unlimited fiscal obligations—they see a Snare where redistribution is pure extraction hidden under rights language. From the vulnerable_populations' seat: Article 3 is a Rope or Tangled_Rope that coordinates genuine subsistence provision and compensates for market failure—they see coordination delivering life-sustaining goods. From the state_enforcement_apparatus' seat: Article 3 is a Scaffold or transitional Rope while welfare systems are built, or a Piton if the founding problem (subsistence deprivation) is solved but enforcement persists theatrically. The reading's truth is not resolved by the seat—the engine computes each seat's classification from the structural data. The gap itself is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (vulnerable_populations, subsistence_access_claimants) derive low directionality (d near 0.0): Article 3 obligates provision to them; they have trapped exit; state action subsidizes their access. Beneficiaries shift from marginalization to legal standing—high-magnitude positive shift. Payers (property_rights_holders, high_income_earners, expression_constrained_actors) derive high directionality (d near 1.0): Article 3 extracts from them via taxation, property restriction, and speech suppression; exit options are constrained or require relocation/capital flight; the constraint's operation targets them directly for resource transfer. The state_enforcement_apparatus sits near d=0.5 (symmetric): it bears institutional responsibility and operational cost, but also collects legitimacy and administrative power from enforcing a foundational right. Negative_liberty_advocates are excluded: they would argue the entire positive entitlement reading is false, making their directionality indeterminate within this framework—they would compute a different constraint entirely (negative_liberty_reading) with opposite beneficiary/victim assignments.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint DOES NOT exhibit mandatrophy at the current interval's end (t=80, roughly year 2020). The founding problem (subsistence deprivation and its role in atrocity) remains live: vulnerable populations still lack adequate access to healthcare, housing, and nutrition in high-inequality states. Positive entitlement enforcement is still active (courts hearing cases, legislatures funding programs), not ceremonial. The theater_ratio is moderate (0.28), indicating real provision machinery exists, not pure performativity. HOWEVER, the omega variable addressing reading_contestation documents an incipiently mandatrophic risk: if the founding problem were to be decisively resolved (say, through automation and UBI solving subsistence access for all populations), the constraint would persist as a legal doctrine without functional necessity, transition into a Piton, and require reclassification. The current status is live Tangled_Rope because enforcement and beneficiary demand remain coupled to the founding problem's active status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contestation_structural,
    'Is Article 3 a positive entitlement (states must provide material conditions) or a negative constraint (states must not deprive except via procedure) or a procedural rule (states must follow due process)? Does the text admit only one coherent reading, or do the three readings represent genuinely live structural alternatives?',
    'Textual analysis grounding each reading in specific Article 3 language + historical-draft analysis showing the drafters'' intent on this question (Humphrey, Cassin, Malik, et al. records) + comparative constitutional law examining how different high-courts interpret parallel provisions (South African, Indian, ECHR) + analysis of whether the three readings'' core axioms can coexist in a single legal framework or logically exclude each other.',
    'If the readings logically exclude each other (one forecloses others), the engine classifies the kernel as over-determined with no coherent foundation—a design failure in the UDHR text. If the readings coexist structurally (different parties hold different readings simultaneously), the kernel sustains genuine multi-reading status and each reading computes as a separate, valid constraint. If the readings'' incommensurability is merely political/ideological rather than logical, they coexist pragmatically and this reading persists as live Tangled_Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_structural, conceptual, 'Whether Article 3''s three readings logically exclude each other, coexist as genuine alternatives, or represent a single indeterminate text being interpreted differently.').

omega_variable(
    material_provision_mandate_scope,
    'Does Article 3''s positive entitlement extend to ALL material conditions necessary for life (food, healthcare, housing, education, transportation, childcare), or only to MINIMAL subsistence? How is the line between ''right to material conditions'' and ''right to economic equality'' drawn?',
    'Constitutional court rulings on positive entitlements (South Africa, India, Germany cases); legislative boundary-setting on welfare-program scope; international human rights body pronouncements (UN CESCR, ECHR) specifying which material conditions Article 3 generates enforceable claims for; economic analysis of what ''life'' minimally requires in different jurisdictions.',
    'If the entitlement scope is narrow (subsistence only, not equality), the extraction is moderate and justified as cost of ensuring survival. If the scope is broad (material conditions necessary for dignity, not just life), the extraction is high and targets middle-class property/income as well as the ultra-wealthy—payer seat directionality increases, effective χ rises. The measurement of extractiveness and the classification''s stability depend heavily on where this boundary is drawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_provision_mandate_scope, empirical, 'The substantive scope of material-condition entitlements Article 3 imposes.').

omega_variable(
    state_capacity_vs_affirmative_obligation,
    'Does Article 3 impose affirmative obligations on states with limited fiscal capacity (and thus generate justiciable claims even when compliance requires resource reallocation), or does Article 3''s obligation scale with state capacity (and thus is non-justiciable in resource-poor jurisdictions)?',
    'International human rights law doctrine on progressive realization (whether Article 3 claims are immediately justiciable or only gradually enforceable) + evidence from high-courts in resource-constrained nations (India, South Africa, Brazil) on whether they enforce Article 3 claims against fiscal constraints + analysis of whether acknowledging capacity limits makes Article 3 non-justiciable (unenforceable against poor states) or weakly justiciable (enforceable to the extent of state capacity).',
    'If Article 3 generates immediate justiciable obligations regardless of state capacity, the constraint applies universally but is massively disobeyed in poor states, creating a legitimacy gap (law/practice divergence). If Article 3 is capacity-scaled, the constraint becomes weaker in poor states and stronger in wealthy ones, creating seat-level divergence between wealthy and poor-state populations—effective extraction differs by jurisdiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_capacity_vs_affirmative_obligation, empirical, 'Whether Article 3 positive entitlements are immediately justiciable or scale with state fiscal capacity.').

omega_variable(
    suppression_of_negative_liberty_claims,
    'To what extent is the positive entitlement reading enforced via suppression of competing negative-liberty claims (property-rights arguments, anti-redistribution speech, tax-evasion defenses)? Is the suppression structural (courts reject negative-liberty standing as matter of precedent) or sustained (courts actively work to suppress negative-liberty advocacy)?',
    'Empirical analysis of court dismissals of property-rights takings claims grounded in Article 3 redistribution + analysis of hate-speech and defamation cases where expression is suppressed as threatening vulnerable populations'' Article 3 security + interviews/testimony from legal actors on whether negative-liberty defenses are rejected categorically or case-by-case.',
    'If suppression is structural (established precedent), the engine classifies the suppression as built-in to the constraint''s architecture, not actively maintained. If suppression is sustained (courts must repeatedly work to reject negative-liberty claims), the suppression metric is higher and the constraint is more coercive. The distinction affects classification: structural suppression is compatible with lower coercion; sustained suppression indicates higher coercion and more active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_negative_liberty_claims, empirical, 'Whether negative-liberty claims against Article 3 redistribution are structurally foreclosed or actively suppressed.').

omega_variable(
    extraction_vector_identity,
    'Who actually extracts the surplus generated by Article 3''s redistribution? Is it vulnerable populations (who receive the material goods), the state apparatus (which controls allocation and captures administrative rents), beneficiary-advocacy groups (which capture constituency power), or some diffuse combination?',
    'Analysis of welfare-state budget allocation: what fraction of extraction goes to direct vulnerable-population provision vs. administrative overhead, advocacy-group funding, political-patronage, and bureaucratic discretion; interviews with vulnerable-population recipients on whether they perceive the constraint as protecting them or as state control over their access; tracking of funding flows from payers to stated beneficiaries vs. intermediaries.',
    'If vulnerable populations are the actual extractors (they receive the goods and control distribution), the constraint is a true Rope or Tangled_Rope with real beneficiaries. If the state apparatus or advocacy groups are the actual extractors (they capture surplus for administrative/political purposes), the constraint is a Snare disguised as entitlement—extraction from both payers AND vulnerable populations, with vulnerable populations as victims too, not beneficiaries. The gain_flow field depends on this answer: either vulnerable_populations or the state-apparatus or diffuse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vector_identity, empirical, 'Whether Article 3''s extracted surplus actually reaches vulnerable populations or is captured by intermediaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(udhr_tr_t10, udhr_article_3__positive_entitlement_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(udhr_tr_t20, udhr_article_3__positive_entitlement_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__positive_entitlement_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(udhr_tr_t40, udhr_article_3__positive_entitlement_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(udhr_tr_t50, udhr_article_3__positive_entitlement_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(udhr_tr_t60, udhr_article_3__positive_entitlement_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(udhr_tr_t70, udhr_article_3__positive_entitlement_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement(udhr_tr_t80, udhr_article_3__positive_entitlement_reading, theater_ratio, 80, 0.28).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(udhr_be_t10, udhr_article_3__positive_entitlement_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(udhr_be_t20, udhr_article_3__positive_entitlement_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__positive_entitlement_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(udhr_be_t40, udhr_article_3__positive_entitlement_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(udhr_be_t50, udhr_article_3__positive_entitlement_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(udhr_be_t60, udhr_article_3__positive_entitlement_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(udhr_be_t70, udhr_article_3__positive_entitlement_reading, base_extractiveness, 70, 0.68).
narrative_ontology:measurement(udhr_be_t80, udhr_article_3__positive_entitlement_reading, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(udhr_su_t10, udhr_article_3__positive_entitlement_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(udhr_su_t20, udhr_article_3__positive_entitlement_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__positive_entitlement_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(udhr_su_t40, udhr_article_3__positive_entitlement_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(udhr_su_t50, udhr_article_3__positive_entitlement_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(udhr_su_t60, udhr_article_3__positive_entitlement_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(udhr_su_t70, udhr_article_3__positive_entitlement_reading, suppression_requirement, 70, 0.42).
narrative_ontology:measurement(udhr_su_t80, udhr_article_3__positive_entitlement_reading, suppression_requirement, 80, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(udhr_article_3__positive_entitlement_reading, 0.16).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, welfare_state_redistributive_apparatus).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, hate_speech_harm_constraint).

% DUAL FORMULATION NOTE:
% Article 3 of the UDHR is a contested kernel admitting at least three structurally distinct readings, each instantiating a different constraint with different ε, beneficiary/victim structures, and types. The POSITIVE ENTITLEMENT READING (this constraint) treats Article 3 as generating affirmative state obligations to provide material conditions—high extraction from property/income, asymmetric transfer to vulnerable populations, active enforcement. The NEGATIVE_LIBERTY_READING (separate constraint) treats Article 3 as limiting state power to deprive life/liberty—low extraction (negative constraint only), focus on procedural protection from state violence. The PROCEDURAL_HYBRID_READING (separate constraint) treats Article 3 as mandating due-process constraints—moderate extraction (meta-rule on enforcement procedure, not substantive rights). These three constraints are linked via network.affects_constraints because they share a kernel (the Article 3 text) and each reading's enforcement affects the others' feasibility. A jurisdiction that enforces positive entitlements aggressively will suppress negative-liberty arguments; one that enforces procedural constraints robustly may bracket the entitlement/liberty dispute. The three are not reducible to one constraint viewed from three angles—they have different ε values, beneficiary/victim structures, and empirical referents (positive entitlement measures fiscal redistribution; negative liberty measures state violence prevention; procedural constraint measures due-process compliance). Each must be authored as a separate, ε-invariant story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
