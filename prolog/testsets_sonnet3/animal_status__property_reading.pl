% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal Legal Personhood Denial — Property/Chattel Reading
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the PROPERTY reading of the contested
 *   animal-status kernel: animals are legal objects, ownership is
 *   unrestricted except by welfare statutes, and no independent moral or
 *   legal standing attaches to the animal. Under this reading's own lights,
 *   the standing arrangement being assessed is the human-to-human allocation
 *   of control rights over animals-as-assets — a genuine, low-overhead
 *   coordination mechanism for allocating a scarce, transferable resource
 *   among owners, breeders, researchers, and traders. Because animals are not
 *   parties to any dispute this reading recognizes, there is no victim set
 *   internal to the reading and extraction is measured only among human
 *   parties (disputed title, breach of sale warranty, welfare-statute
 *   noncompliance between owner and regulator) — hence the near-zero ε. This
 *   is a DELIBERATE and NECESSARY divergence from the welfare_reading and
 *   abolitionist_reading siblings, which are separate constraint files with
 *   their own ε values and their own victim sets (animals, under those
 *   readings). Per the ε-invariance principle, this is not the same
 *   constraint measured differently — it is a different constraint, because a
 *   different party set is recognized as capable of bearing costs.
 *
 * KEY AGENTS:
 *   - animal_owners: primary beneficiary (moderate/mobile) — holds title, captures use-value
 *   - agricultural_producers: organized beneficiary (organized/arbitrage) — industrial-scale capture, lobbies to keep the frame narrow
 *   - biomedical_research_industry: institutional beneficiary (institutional/mobile) — procedural rather than adversarial oversight
 *   - animal_welfare_advocates: excluded party (organized/constrained) — litigates for standing, repeatedly denied under this frame
 *   - courts_and_legislatures: agenda_setter (institutional/analytical) — administers and could revise the category boundary but has not
 *   - companion_and_farmed_animals: non-agent excluded party under this reading's own terms — listed for completeness only
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.06).
domain_priors:suppression_score(animal_status__property_reading, 0.15).
domain_priors:theater_ratio(animal_status__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Legal Personhood Denial — Property/Chattel Reading").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '067eef92-6ce9-41ea-8b42-df9ed12733c8').
narrative_ontology:cs_kernel_codification('067eef92-6ce9-41ea-8b42-df9ed12733c8', formalized).
narrative_ontology:cs_authority_grounding('067eef92-6ce9-41ea-8b42-df9ed12733c8', lineage).
narrative_ontology:cs_interpretation_layer_present('067eef92-6ce9-41ea-8b42-df9ed12733c8').
narrative_ontology:cs_reading_relation('067eef92-6ce9-41ea-8b42-df9ed12733c8', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('067eef92-6ce9-41ea-8b42-df9ed12733c8', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('067eef92-6ce9-41ea-8b42-df9ed12733c8', foundational, human_exclusive_moral_patienthood).
narrative_ontology:cs_axiom_status(human_exclusive_moral_patienthood, holdable).
narrative_ontology:cs_axiom_grounding('067eef92-6ce9-41ea-8b42-df9ed12733c8', human_exclusive_moral_patienthood, conventional).
narrative_ontology:cs_axiom('067eef92-6ce9-41ea-8b42-df9ed12733c8', secondary, welfare_statute_as_sufficient_limit).
narrative_ontology:cs_axiom_status(welfare_statute_as_sufficient_limit, holdable).
narrative_ontology:cs_axiom_grounding('067eef92-6ce9-41ea-8b42-df9ed12733c8', welfare_statute_as_sufficient_limit, instrumental).
narrative_ontology:cs_reference_frame('067eef92-6ce9-41ea-8b42-df9ed12733c8', common_law_chattel_status).
narrative_ontology:cs_drift_state('067eef92-6ce9-41ea-8b42-df9ed12733c8', post_sentience_science_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('067eef92-6ce9-41ea-8b42-df9ed12733c8', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, agricultural_producers).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, biomedical_research_industry).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, pet_trade_operators).
narrative_ontology:constraint_vindicates(animal_status__property_reading, human_exclusive_moral_patienthood).
narrative_ontology:constraint_vindicates(animal_status__property_reading, property_law_sufficiency_for_animal_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold full legal title over animals as chattel: may buy, sell, breed, confine, or destroy them subject only to welfare-statute minimums. The property frame gives them clear, low-cost, court-enforceable title and insulates ordinary use decisions from moral or legal challenge by third parties.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_owners, beneficiary,
    moderate, biographical, mobile, national).

% Operate industrial-scale animal use (confinement, slaughter, breeding) whose economics depend on animals carrying no standing to sue, no independent claim to damages, and no interest that must be weighed against production efficiency beyond statutory minimums. Lobby to keep welfare statutes narrow and to keep any personhood or standing claim out of court.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, agricultural_producers, beneficiary,
    organized, generational, arbitrage, national).

% Uses animals in experimentation under IACUC-style welfare oversight but without any recognition of the animal as a party with legal interests. Regulatory compliance is procedural (protocol review) rather than adversarial (no animal-side legal representation), which keeps research costs and timelines predictable.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, biomedical_research_industry, beneficiary,
    institutional, generational, mobile, national).

% Breed and sell companion animals as merchandise; the property frame lets transactions clear through ordinary sales law (warranty, replacement, return) rather than custody or best-interest standards that would apply to a rights-holder.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, pet_trade_operators, beneficiary,
    moderate, biographical, mobile, national).

% Litigate for expanded standing (guardianship suits, habeas corpus for chimpanzees, standing-to-sue statutes) and are repeatedly turned back by courts applying the property frame, which treats their clients as objects incapable of holding a cause of action. They are present in the discourse but structurally locked out of the adjudicating category.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_welfare_advocates, excluded,
    organized, generational, constrained, national).

% Under this reading, animals hold no legal interest to be weighed and are not parties to any dispute the constraint recognizes — their situation is described here for completeness, not as a stakeholder claim, since the reading itself denies them agent status.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, companion_and_farmed_animals, excluded,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(animal_status__property_reading, companion_and_farmed_animals).

% Adjudicate property disputes over animals and enact/enforce welfare statutes as the sole limit on ownership rights. They administer the category boundary — deciding, case by case, that animals remain objects rather than parties — and could in principle expand standing but have consistently declined to.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__property_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, low-cost, well-understood legal category (chattel property) for allocating control over animals among humans, avoiding the transaction costs of a novel legal status and letting existing property, tort, and contract law govern all animal-related disputes.
% TRANSFER_FUNCTION: Moves the entire surplus of animal use — labor, flesh, companionship, research value — to the human owner or user, subject only to the floor set by welfare statutes; nothing is owed to the animal because the animal is not a party who can be owed anything under this reading.
% ABSENT_VOICES: The animals themselves have no voice by the reading's own terms — they are the excluded party by design, not by oversight. Animal welfare and rights advocates are present in public discourse but are structurally excluded from the adjudicating legal category itself: their arguments are heard as policy input, never as claims a court can grant standing to under the property frame.
% DISAPPEARANCE_RATIONALE: If the property classification vanished overnight and were not replaced by another kernel reading, every existing sale, breeding, slaughter, and research contract involving animals would lose its legal basis; ownership records, insurance, secured lending against livestock, and entire industries built on animals-as-assets would require immediate re-grounding in some other legal category.
% FOUNDING_PROBLEM: Historically, legal systems needed a settled way to allocate control over animals among humans — for husbandry, transport, trade, and labor — without relitigating the animal's moral status in every transaction; treating animals as property provided a stable, transferable, court-enforceable title.
% FOUNDING_PROBLEM_CORROBORATION: Agricultural and research industry representatives attest the founding problem (need for stable, transferable title enabling commerce) remains live and well-served. Legal scholars and welfare/rights litigators outside the beneficiary set attest that the category now functions primarily to shield large-scale industrial use from liability and standing claims that scientific findings on animal sentience would otherwise support — i.e., the original husbandry-title problem is largely solved by modern secured-transactions law generally, and the property frame's remaining work is liability insulation rather than transactional necessity.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).
:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.05-0.06) because, by this reading's own terms, there is no recognized party from whom value is extracted beyond ordinary human-to-human commercial friction (contract breach, warranty disputes) — the reading does not recognize the animal as a locus of cost-bearing at all. Suppression is low-moderate (0.15) reflecting the mild friction welfare statutes impose on absolute ownership, not any coercive apparatus suppressing an internal victim class. Theater ratio is low and rises only slightly over the interval (0.05 to 0.10), tracking the modest growth of welfare-compliance paperwork (inspection regimes, certification labels) that performs concern without altering the underlying property structure. Accessibility collapse is moderate (0.35): alternative legal categorizations (guardianship, qualified personhood, standing-to-sue statutes) are visible and actively litigated, so alternatives have NOT collapsed the way they would for a genuine mountain — this is a contested, revisable legal choice, not a natural law. Resistance is moderate (0.4), reflecting the sustained, organized welfare/rights litigation campaign against the frame — a mountain would show near-zero resistance; this reading's persistence requires active legal defense.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners, agricultural producers, the research industry, and pet-trade operators are declared beneficiaries: the property frame gives them clear, cheap, court-enforceable title and insulates ordinary use from third-party or animal-side claims — they sit near the full-beneficiary end of directionality. Courts and legislatures are the agenda_setter: they administer the category and could expand standing but have institutional and path-dependent reasons not to. Welfare advocates are excluded rather than victimized in the technical sense used here — they bear no direct extraction under this reading, but are structurally locked out of the adjudicating category, which is a distinct harm (denial of voice, not denial of a claimed share of a transfer) captured in absent_voices rather than in the victims array.
 *
 * MANDATROPHY ANALYSIS:
 *   The property reading's founding problem — providing a stable, transferable title for allocating control over animals among humans — was live when animal husbandry required a settled legal category distinct from real property or human status. That transactional problem is largely solved by general secured-transactions and commercial law today; what the property frame's exclusivity now principally does is insulate industrial-scale use from liability claims that expanding scientific consensus on animal sentience would otherwise support. This divergence between original function (title-clarity) and current function (liability insulation) is exactly the founding_problem_status: contested signal — it is not dispositive of mandatrophy on its own, since the title-clarity function still operates for ordinary pet ownership and small-scale husbandry; it is a live tension the corpus should track rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_moral_patienthood_ambiguity,
    'Is the exclusion of animals from the recognized party set a defensible metaphysical/moral claim (animals lack the relevant capacities for moral standing) or a constructed legal convenience that benefits identifiable commercial actors regardless of the underlying moral facts?',
    'This is likely irreducible at the empirical level — it depends on unresolved questions in philosophy of mind (sentience, interests, moral status) that no single data-gathering exercise settles. Partial resolution: track whether jurisdictions granting expanded standing (e.g., qualified personhood for cetaceans, standing-to-sue statutes) show different downstream welfare outcomes than pure-property jurisdictions, holding industry type constant.',
    'If resolved toward ''constructed convenience,'' the property reading''s ε ~0.05 should be read as an artifact of an under-inclusive party set rather than a true absence of extraction — the abolitionist_reading''s much higher ε would be the more accurate account of the same underlying practices. If resolved toward ''defensible moral claim,'' the property reading''s low ε stands on firmer ground.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(animal_moral_patienthood_ambiguity, conceptual, 'Whether animal exclusion from party status is philosophically grounded or a beneficiary-serving legal convenience.').

omega_variable(
    welfare_statute_floor_erosion,
    'Do welfare statutes, which are this reading''s sole limit on ownership, actually bind in practice, or are they systematically under-enforced such that the ''except by welfare statutes'' clause is substantially theatrical?',
    'Compare statutory welfare standards against enforcement rates, inspection frequency, and penalty severity across agricultural, research, and companion-animal sectors; rising theater_ratio in the measurement series would corroborate under-enforcement.',
    'If welfare statutes are largely unenforced, the property reading''s already-low ε may still overstate the real constraint on owners, and the reading shades toward piton (a nominal limit maintained mostly for legitimacy rather than binding force) rather than a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_floor_erosion, empirical, 'Whether the welfare-statute floor is a real binding constraint or largely performative.').

omega_variable(
    sibling_reading_boundary_location,
    'Where exactly does the disagreement between property_reading and welfare_reading sit structurally — is it a disagreement about facts (do animals have morally relevant interests) or about legal technique (should interests, once granted, translate into standing to sue)?',
    'Track jurisdictions transitioning from property to welfare-style frameworks and identify whether the transition was triggered by new scientific findings (factual) or by procedural reform (standing statutes) absent new findings (technique).',
    'If the disagreement is purely technical, the two readings are closer than their ε gap suggests and convergence is plausible; if factual, the readings are genuinely incommensurable absent resolution of the sentience question, and the kernel remains a live three-way contest indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_location, conceptual, 'Locating the structural axis of disagreement between property and welfare readings of the animal_status kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t8, animal_status__property_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(anim_tr_t16, animal_status__property_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement(anim_tr_t24, animal_status__property_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(anim_tr_t32, animal_status__property_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement(anim_tr_t40, animal_status__property_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(anim_be_t8, animal_status__property_reading, base_extractiveness, 8, 0.045).
narrative_ontology:measurement(anim_be_t16, animal_status__property_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement(anim_be_t24, animal_status__property_reading, base_extractiveness, 24, 0.055).
narrative_ontology:measurement(anim_be_t32, animal_status__property_reading, base_extractiveness, 32, 0.06).
narrative_ontology:measurement(anim_be_t40, animal_status__property_reading, base_extractiveness, 40, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_status__property_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status__property_reading, 0.1).
narrative_ontology:affects_constraint(animal_status__property_reading, welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the animal_status kernel. property_reading, welfare_reading, and abolitionist_reading share the same underlying textual/practical kernel (what legal and moral status animals hold) but instantiate structurally distinct constraints with different recognized party sets and therefore different ε: property_reading ~0.05 (no animal-side victim set), welfare_reading moderate (animals recognized as interest-holders but not rights-holders — constrained, not prohibited, use), abolitionist_reading high (the entire instrumental-use regime read as extraction from a recognized victim class). Each file must be read independently; do not average or reconcile their ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
