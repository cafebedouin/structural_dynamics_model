% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause as Text-Limited Interstate Transaction Authority (Originalist Reading)
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   The Commerce Clause ('Congress shall have Power...To regulate
 *   Commerce...among the several States') is a contested kernel in American
 *   constitutional law. This constraint story instantiates the ORIGINALIST
 *   NARROW READING: federal commerce authority extends only to transactions
 *   crossing state borders and instrumentalities of interstate movement;
 *   states retain police power over intrastate activity; the clause does not
 *   authorize federal regulation of local production, manufacturing, or
 *   transactions where goods remain within a single state. This reading
 *   directly confronts two centuries of jurisprudence (Wickard v. Filburn,
 *   1942; Gonzales v. Raich, 2005) that permitted federal authority to reach
 *   intrastate activity having 'substantial effects' on interstate commerce.
 *   The originalist reading constrains federal power and returns regulatory
 *   authority to state governments. It benefits state sovereignty advocates
 *   and federalism-focused constitutional theorists. It extracts cost from
 *   uniform interstate market standards and from management of negative
 *   externalities that cross state lines but originate in intrastate
 *   activity.
 *
 * KEY AGENTS:
 *   - State Governments: Primary beneficiary (institutional/arbitrage) — retain police power, regulatory autonomy, tax and tariff authority over local commerce
 *   - Anti-Federalization Coalition: Beneficiary (institutional/arbitrage) — constitutional originalists, federalism scholars, state sovereignty advocates; see the narrow reading as solution to federal consolidation
 *   - Interstate Market Uniformity: Primary victim (powerless/trapped) — abstract collective good; cannot organize, cannot exit, cannot appeal to federal authority for standards harmonization
 *   - Negative Externality Management: Victim (powerless/trapped) — pollution, health hazards, resource depletion crossing state lines; bears costs of harms that originate outside its jurisdiction and cannot be federally regulated
 *   - Other States (Downstream/Affected): Secondary victim (moderate/constrained) — face spillover harms from intrastate activity in origin states; constrained by inability to federally require origin-state regulation
 *   - Federal Regulatory Apparatus: Institutional observer (institutional/constrained) — historically built on expansive commerce power; originalist reading constrains its authority and forces devolution to state-level mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.38).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.52).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause as Text-Limited Interstate Transaction Authority (Originalist Reading)").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '36971ebd-927a-4ece-99e6-61add2ebe6e6').
narrative_ontology:cs_kernel_codification('36971ebd-927a-4ece-99e6-61add2ebe6e6', fixed_text).
narrative_ontology:cs_authority_grounding('36971ebd-927a-4ece-99e6-61add2ebe6e6', lineage).
narrative_ontology:cs_interpretation_layer_present('36971ebd-927a-4ece-99e6-61add2ebe6e6').
narrative_ontology:cs_reading_relation('36971ebd-927a-4ece-99e6-61add2ebe6e6', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('36971ebd-927a-4ece-99e6-61add2ebe6e6', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('36971ebd-927a-4ece-99e6-61add2ebe6e6', foundational, constitutional_text_has_fixed_semantic_meaning).
narrative_ontology:cs_axiom_status(constitutional_text_has_fixed_semantic_meaning, holdable).
narrative_ontology:cs_axiom_grounding('36971ebd-927a-4ece-99e6-61add2ebe6e6', constitutional_text_has_fixed_semantic_meaning, deontological).
narrative_ontology:cs_axiom('36971ebd-927a-4ece-99e6-61add2ebe6e6', foundational, federal_consolidation_threat_requires_constitutional_limit).
narrative_ontology:cs_axiom_status(federal_consolidation_threat_requires_constitutional_limit, holdable).
narrative_ontology:cs_axiom_grounding('36971ebd-927a-4ece-99e6-61add2ebe6e6', federal_consolidation_threat_requires_constitutional_limit, instrumental).
narrative_ontology:cs_reference_frame('36971ebd-927a-4ece-99e6-61add2ebe6e6', text_constrained_federal_authority).
narrative_ontology:cs_drift_state('36971ebd-927a-4ece-99e6-61add2ebe6e6', contemporary_post_substantial_effects_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36971ebd-927a-4ece-99e6-61add2ebe6e6', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federalization_coalitions).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, interstate_market_uniformity).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, negative_externality_management).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERSTATE MARKET UNIFORMITY (SNARE) — Under originalist narrow reading, interstate commerce coordination cannot be federally mandated when goods remain intrastate or transactions are technically local. Market uniformity has no advocate, no exit option, and bears full cost of balkanization. The epistemic commons of uniform standards becomes trapped in state-by-state variation with no federal remedy.
constraint_indexing:constraint_classification(commerce_clause_text__originalist_narrow_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEGATIVE EXTERNALITY MANAGEMENT (SNARE) — Pollution, resource depletion, health hazards that cross state lines cannot be addressed federally if the harmful activity is deemed intrastate under originalist framing. The victims of externalities — future generations, downstream states, diffuse publics — cannot organize, exit, or appeal to federal authority. Maximum extraction: bear costs of harms that originate outside their control and cross borders the originalist reading forbids federal intervention to manage.
constraint_indexing:constraint_classification(commerce_clause_text__originalist_narrow_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE GOVERNMENTS (TANGLED ROPE) — States benefit from retained police power and regulatory autonomy over intrastate commerce under the originalist reading. But states also bear coordination costs when fragmented regulations create deadweight loss and when they face interjurisdictional spillovers (pollution, labor arbitrage, capital flight) they cannot federally address. The originalist constraint enables state extraction from internal actors but constrains state ability to manage cross-border collective action problems. Constrained exit because states that want uniform interstate standards lack federal authority to impose them.
constraint_indexing:constraint_classification(commerce_clause_text__originalist_narrow_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANTI-FEDERALIZATION COALITION (ROPE) — Constitutional originalism, federalism-focused legal scholarship, and state sovereignty advocates see the narrow reading as solving a coordination problem: how to preserve state autonomy against federal consolidation. From this perspective, the constraint is pure coordination — articulating a rule (federal authority limited to border-crossing) that prevents a collective action trap (states individually agree to federal authority, then federal authority expands beyond agreement). Institutional power + arbitrage exit (can exit by adopting expansive reading) produces rope classification.
constraint_indexing:constraint_classification(commerce_clause_text__originalist_narrow_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSTITUTIONAL TEXT INTERPRETATION APPARATUS (PITON) — The originalist narrow reading persists as an interpretive tradition despite 100+ years of Supreme Court jurisprudence (Wickard, Gonzales, etc.) that expanded federal authority beyond the text's literal scope. The originalist position maintains itself through academic networks, judicial philosophy, and constitutional theory, but the operational constraint (federal authority limited to border-crossing) has been theater for decades — the actual law governs intrastate activity when Congress invokes the substantial effects doctrine. Theater ratio high (0.68): the originalist reading performs a disciplinary function (constraining interpretation drift) but does not constrain actual federal authority. Piton: degraded constraint maintained by institutional inertia.
constraint_indexing:constraint_classification(commerce_clause_text__originalist_narrow_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TEXTUAL IMMUTABILITY (MOUNTAIN) — From a civilizational perspective treating the Constitution's text as fixed and its meaning as determinate, the originalist reading appears to derive from an immutable property of language itself: the words 'commerce among the several states' have a meaning that excludes purely intrastate activity by logical necessity. This perspective risks naturalizing a contested interpretive choice as a textual given. The engine's false summit detector will identify this: the mountain classification naturalizes what is actually a hermeneutic claim, not a logical limit.
constraint_indexing:constraint_classification(commerce_clause_text__originalist_narrow_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commerce_clause_text__originalist_narrow_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commerce_clause_text__originalist_narrow_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, TR),
    TR >= 0.70.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high, reflecting the constraint's asymmetric structure. The originalist reading extracts from interstate market coordination (uniform standards impossible) and from negative externality management (cross-border harms unaddressable) while benefiting state regulatory autonomy. The extraction is not maximal (snare-level ≥0.46) because some coordination still occurs through state-level mechanisms and interstate compacts, and because the originalist reading is itself contested — it does not fully foreclose federal authority (substantial effects doctrine remains a live alternative). The measurement trajectory (0.08 → 0.38 over 239 years) shows rising extractiveness as the gap between originalist constraint and actual federal practice widened. Suppression (0.52): Significant barriers to challenging the originalist constraint include constitutional text (the narrow reading has plausible textual support), constitutional theory (originalism is intellectually serious), and federalism political economy (state governments prefer the reading). But suppression is not total (0.60+) — the expansive reading remains dominant in Supreme Court doctrine, and federal regulatory practice frequently reaches intrastate activity. Theater ratio (0.68): High and rising. The originalist narrow reading performs disciplinary work (constrains interpretation drift, maintains fidelity to constitutional text) but does not operationally constrain federal authority — the substantial effects doctrine and other doctrinal moves allow federal regulation of intrastate activity despite the originalist text. Theater increased as the gap between originalist constraint and actual federal authority grew (1787–1920, theater low because originalist constraint matched practice; 1920–2026, theater high because originalist constraint diverged from practice).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits all six classification types, revealing the complete perspectival structure of constitutional interpretation. Interstate market uniformity and negative externality management see pure extraction (Snare) — they have no advocate and cannot exit. State governments see mixed coordination and extraction (Tangled Rope) — they benefit from autonomy but face coordination costs and interjurisdictional spillovers. Anti-federalization coalition sees pure coordination (Rope) — solving the problem of federal consolidation. Constitutional interpretation apparatus sees degraded doctrine (Piton) — the originalist reading persists through academic networks and judicial philosophy but does not operationally constrain federal authority. The analytical observer risks seeing textual immutability (Mountain) — treating the originalist reading as a logical entailment of the text rather than an interpretive choice. The false summit detector will identify this: the mountain classification naturalizes what is actually a hermeneutic claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from beneficiary/victim status plus exit options. State governments as beneficiaries with arbitrage options (can exit by adopting expansive reading) experience low d (~0.15–0.25), producing negative or near-zero χ. Interstate market uniformity as a powerless trapped victim experiences high d (~0.90–0.95), producing maximum χ. Anti-federalization coalition as organized beneficiary with arbitrage exit experiences low d (~0.10–0.20). Negative externality management as powerless trapped victim experiences maximum d (~0.95), maximum χ. The analytical observer uses canonical d for analytical power (0.72–0.73), producing moderate-high χ and rope-piton classifications at lower time horizons. The core directionality insight: the originalist reading benefits concentrated, organized state-level actors with exit options (low d) and harms diffuse, unorganizable interstate public goods (high d). This asymmetry produces the moderate-high extractiveness score (0.38) — extraction concentrates on victims who cannot exit and benefit does not concentrate enough to produce snare-level suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy through the divergence between constitutional text and constitutional practice. The originalist narrow reading emerges as mountain (from analytical perspective) or rope (from federalism coalition perspective) when you treat the constitutional text as fixed and determinative. But the base_extractiveness measurement (0.38) is driven by the gap between the reading's constraint and actual federal practice. The substantial effects doctrine, established in Wickard (1942) and reaffirmed in Gonzales (2005), permits federal regulation of intrastate activity — de facto resolving the mandatrophy by rejecting the narrow reading as a binding constraint. The originalist reading persists as piton (theater) and mountain (naturalized constraint) precisely because it has been doctrinally resolved: the Supreme Court has chosen the expansive reading, and the narrow reading maintains itself through academic networks and dissenting opinions rather than binding legal authority. The mandatrophy is therefore resolved at the level of positive constitutional law — the expansive reading won — but unresolved at the level of constitutional theory and interpretation — originalists maintain that the narrow reading is texturally mandated and the Supreme Court's expansion was illegitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentality_boundary_underdetermined,
    'What counts as an ''instrumentality of interstate commerce''? Does manufacturing equipment qualify if inputs cross state lines? What about intrastate transportation of goods ultimately destined for interstate movement?',
    'Case law analysis: track Supreme Court holdings on what activity counts as regulation of ''instruments of interstate commerce'' (dormant Commerce Clause and affirmative federal commerce power cases). Identify the boundary that case law treats as controlling.',
    'If boundary is strict (only goods actually in transit across borders): extractiveness remains high (0.38–0.45), snare classifications persist. If boundary is expansive (all activity substantially affecting interstate flow): extractiveness drops to 0.15–0.25, rope classifications dominate, originalist reading collapses into piton (theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(instrumentality_boundary_underdetermined, empirical, 'Boundary definition for ''instrumentality of interstate commerce''').

omega_variable(
    originalist_semantics_vs_constitutional_function,
    'Does the originalist narrow reading prioritize semantic fidelity to 18th-century meaning of ''commerce'' (commercial exchange, excluding regulation of production) or does it incorporate the Framers'' functional intent (federal authority sufficient to solve interstate coordination problems)?',
    'Historical scholarship: evidence from Founding-era sources about whether Framers intended narrow semantic meaning or broad functional grant. Compare originalist vs functionalist constitutional scholarship on Framers'' intent. Examine whether originalist doctrine actually applies 18th-century semantics consistently or implicitly reads in modern exceptions.',
    'If semantic: originalist reading is internally coherent but structurally brittle — any expansion (substantial effects test) is a betrayal. If functional: originalist reading dissolves into the expansive reading, and the constraint''s ε drops to near zero (all readings agree on functional grant; they only disagree on scope). If inconsistent: originalists smuggle in functional exceptions, reducing the extractiveness of the narrow reading''s constraint on federal power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_semantics_vs_constitutional_function, conceptual, 'Whether originalism privileges text semantics or Framers'' functional intent').

omega_variable(
    dormant_commerce_clause_interaction,
    'Under the originalist narrow reading of affirmative federal commerce authority, does the dormant Commerce Clause (state action limits) have greater or lesser constraining force? If federal authority is narrower, do state police powers expand accordingly, or do dormant limits tighten to prevent state interference with whatever interstate commerce remains?',
    'Doctrinal analysis: map how originalist scholars reconcile narrow federal commerce authority with dormant Clause doctrine. Identify whether the reading treats dormant limits as enforced against expanded state power (tight dormant review) or as secondary to expanded state police power (loose dormant review). Case law pattern analysis: do courts applying originalist logic show higher or lower willingness to uphold state regulations under dormant Clause challenge?',
    'If dormant limits tighten to compensate: federal authority is narrow but state regulatory power is also constrained — market fragmentation is prevented by dormant enforcement. Snare and piton classifications attenuate; tangled_rope becomes clearer (state governments actually benefit from dormant limits on competitor states). If dormant limits relax: federal authority is narrow AND state power expands — market fragmentation becomes severe. Snare classifications intensify; externality management (victim) classification sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormant_commerce_clause_interaction, empirical, 'Interaction between narrow federal commerce authority and dormant Commerce Clause limits').

omega_variable(
    naturalizing_interpretive_choice_as_constitutional_law,
    'Is the originalist narrow reading a description of what the Constitution *requires* (textualist constraint on legitimate interpretation) or a normative proposal about how the Constitution *should* be read (interpretive choice among live options)? This is the core kernel decomposition question: whether the reading is a discovered feature of the text or a construction built upon it.',
    'Constitutional theory analysis: examine whether originalist doctrine treats the narrow reading as mandatory (text logically entails it) or justified (text supports it better than alternatives). Track whether originalist scholars acknowledge that the expansive reading is textually defensible but disfavored on originalist grounds, or claim the expansive reading is textually indefensible. Case law: do originalist Supreme Court opinions claim to have no choice in interpretation or present the narrow reading as one legitimate option?',
    'If discovered (mandatory): mountain classification is appropriate (the text constrains interpretation). If construction (choice): false summit signature fires — the originalist reading naturalizes an interpretive decision as a textual given. The constraint''s claimed type shifts from mountain to tangled_rope or piton. The omega itself is the structural difference this reading instantiates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalizing_interpretive_choice_as_constitutional_law, conceptual, 'Whether originalist narrow reading is texturally mandated or interpretively chosen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1787, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_founding_era, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1787, 0.15).
narrative_ontology:measurement(theater_progressive_era, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1920, 0.42).
narrative_ontology:measurement(theater_modern_era, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1960, 0.65).
narrative_ontology:measurement(theater_contemporary, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2026, 0.68).

% Extraction over time
narrative_ontology:measurement(extract_founding_era, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1787, 0.08).
narrative_ontology:measurement(extract_progressive_era, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1920, 0.18).
narrative_ontology:measurement(extract_modern_era, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1960, 0.32).
narrative_ontology:measurement(extract_contemporary, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, dormant_commerce_clause_state_regulation).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, federalism_regulatory_authority_division).

% DUAL FORMULATION NOTE:
% The commerce clause kernel decomposes into three distinct constraint stories corresponding to three live constitutional readings. Each reading has different ε, different beneficiary/victim structures, and different classification profiles. The originalist narrow reading (this constraint, ε=0.38) constrains federal authority and benefits state governments; the expansive federal reading (ε=0.05–0.12) enables federal market uniformity and extracts less from state autonomy; the substantial effects limited reading (ε=0.20–0.28) represents a middle position attempting to balance both concerns. Each story is ε-invariant within its interpretive commitment; the readings differ in what they measure, not in perspectival multiplicity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
