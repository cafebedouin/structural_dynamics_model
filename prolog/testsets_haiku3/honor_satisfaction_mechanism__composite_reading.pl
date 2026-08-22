% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism (Composite: State Monopoly, Bourgeois Norms, Insurance, Category-Shift)
 *   domain: legal/normative/social
 *
 * SUMMARY:
 *   From 1750 to 1900, honor-satisfaction through dueling was progressively
 *   de-legitimized and criminalized across Europe, not by a single mechanism
 *   but by the synchronized operation of four distinct systems: state legal
 *   monopoly over violence and dispute resolution; bourgeois cultural norms
 *   that reframed honor-vindication as irrational barbarism; insurance
 *   markets that priced dueling into financial catastrophe; and a fundamental
 *   category-shift that moved honor from an objective social fact (public
 *   insult requiring visible vindication) to a subjective-psychological state
 *   (adequately addressed through internal acceptance or legal remedy). This
 *   constraint story instantiates the composite_reading of the
 *   honor_satisfaction_mechanism kernel — the reading that honor prohibition
 *   succeeded because multiple independent extractive mechanisms attacked it
 *   simultaneously, each benefiting different institutional actors (state,
 *   bourgeoisie, insurers) while imposing costs on military aristocracy and
 *   those for whom honor-satisfaction through violence was
 *   identity-constitutive. The sibling readings (contraction_reading: dueling
 *   became cognitively unthinkable; decline_reading: dueling simply declined
 *   in frequency) are coexistent competing interpretations of the same
 *   historical record, not foreclosed by this reading.
 *
 * KEY AGENTS:
 *   - State authority: enforces prohibition, claims monopoly over honorable dispute resolution
 *   - Bourgeois professional class: advances norms treating honor-violence as irrational, incompatible with commerce
 *   - Insurance underwriters: exclude duelists from coverage, making participation financially catastrophic
 *   - Military aristocracy: bearing criminal, social, and identity costs of prohibition
 *   - Urban lower classes: facing uneven enforcement of prohibition while never having had access to dueling mechanism
 *   - Legal interpreters: shift doctrine from objective (public insult) to subjective (internal state) honor
 *   - Cultural reformers: narrative displacement of dueling from honorable to pathological
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.71).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Composite: State Monopoly, Bourgeois Norms, Insurance, Category-Shift)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "legal/normative/social").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '02a735d5-7344-4d20-a663-49de9bf522c5').
narrative_ontology:cs_kernel_codification('02a735d5-7344-4d20-a663-49de9bf522c5', formalized).
narrative_ontology:cs_authority_grounding('02a735d5-7344-4d20-a663-49de9bf522c5', extraction).
narrative_ontology:cs_interpretation_layer_present('02a735d5-7344-4d20-a663-49de9bf522c5').
narrative_ontology:cs_reading_relation('02a735d5-7344-4d20-a663-49de9bf522c5', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('02a735d5-7344-4d20-a663-49de9bf522c5', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_axiom('02a735d5-7344-4d20-a663-49de9bf522c5', foundational, honor_satisfaction_requires_institutional_extraction).
narrative_ontology:cs_axiom_status(honor_satisfaction_requires_institutional_extraction, holdable).
narrative_ontology:cs_axiom_grounding('02a735d5-7344-4d20-a663-49de9bf522c5', honor_satisfaction_requires_institutional_extraction, empirically_contingent).
narrative_ontology:cs_axiom('02a735d5-7344-4d20-a663-49de9bf522c5', secondary, multiple_mechanisms_synchronized_enforcement).
narrative_ontology:cs_axiom_status(multiple_mechanisms_synchronized_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('02a735d5-7344-4d20-a663-49de9bf522c5', multiple_mechanisms_synchronized_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('02a735d5-7344-4d20-a663-49de9bf522c5', aristocratic_honor_vindication_system).
narrative_ontology:cs_drift_state('02a735d5-7344-4d20-a663-49de9bf522c5', contemporary_industrial_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('02a735d5-7344-4d20-a663-49de9bf522c5', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_authority).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_underwriters).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, military_aristocracy).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, urban_lower_classes).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, excluded_duelists).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, state_monopoly_over_violence).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, rational_dispute_resolution).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, bourgeois_normativity_as_legal_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces legal prohibition on dueling, prosecutes duelists, claims the state's exclusive right to adjudicate honor disputes through courts. Benefits from concentrating violence under state control and delegitimizing alternative justice mechanisms. Requires continuous enforcement against aristocratic and military resistance.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_authority, agenda_setter,
    institutional, generational, analytical, national).

% Advances legal and cultural norms reframing honor disputes as internal psychological states rather than matters requiring violent vindication. Their framework treats dueling as irrational, barbaric, and incompatible with commercial reliability and professional standing. Gains legitimacy for bourgeois norms as the legal and cultural standard replacing aristocratic codes.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).

% Refuse life insurance and liability coverage to duelists and those who engage in honor violence, making it financially catastrophic to participate in the dueling system. The prohibition becomes structurally encoded into economic rationality. Collects rents on insurance as participants must obtain coverage by abandoning dueling.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_underwriters, beneficiary,
    organized, biographical, mobile, national).

% Faces criminal prosecution and social exclusion for defending honor through dueling, the mechanism historically required to maintain status and masculine identity in their rank. Their exit from dueling entails abandoning the identity framework within which honor operates. Constrained by both legal prohibition and identity-fusion: to accept state courts is to renounce their definition of honor itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, military_aristocracy, payer,
    powerful, generational, identity_locked, national).

% Never had formal access to dueling as a satisfaction mechanism (gentlemen-only institution), but face legal and economic consequences when informal honor disputes escalate to violence. The prohibition on dueling is written in upper-class language and enforced unevenly — lower-class violence is criminalized while aristocratic and military honor claims historically escaped prosecution. They bear the costs of criminalization without having benefited from the original mechanism.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, urban_lower_classes, payer,
    powerless, immediate, trapped, local).

% Individuals who continue engaging in dueling despite prohibition face criminal penalties, loss of employment, insurance exclusion, and social ostracism. Their options are abandoning honor vindication entirely (accepting legal court outcomes, which they regard as inadequate), accepting criminal consequences, or operating in the margins and losing institutional affiliation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, excluded_duelists, payer,
    moderate, biographical, constrained, national).

% Judges and legal scholars interpret and apply honor prohibition laws, gradually shifting legal doctrine from treating honor as a matter of objective fact (public insult) to treating it as a subjective-internal matter (the insulted person's psychological state). This reframing enables courts to dismiss honor grievances as not justifying violence, delegitimizing dueling on epistemic grounds.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_interpretation_community, observer,
    institutional, generational, analytical, national).

% Intellectual and literary figures advance narratives portraying dueling as barbaric, wasteful, and incompatible with civilization and progress. They reframe participants not as honorable men defending their worth but as victims of an irrational system — repositioning dueling as a pathology rather than a legitimate practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, cultural_reformers, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, state_authority).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Providing a mechanism to resolve public honor disputes and restore reputation after insult — historically the dueling system itself (second witnesses, agreed protocols, recognized outcomes). The state-bourgeois constraint replaces this with court proceedings and internal psychological resolution, claiming these coordinate reputation repair without violence.
% TRANSFER_FUNCTION: Transfers the power to define and adjudicate honor from individual/aristocratic codes to state legal authority and bourgeois normative standards. Simultaneously transfers insurance and legal compliance costs to those who persist in honor-vindication outside the state system. The constraint moves the locus of honor-satisfaction from violent practice to compliance with non-violent institutional procedures.
% ABSENT_VOICES: Military and aristocratic officers who view honor-vindication through violence as non-negotiable are structurally excluded from the legal conversation — prosecution precedes dialogue. Lower-class participants in informal violence have no seat at all: the constraint was framed as an upper-class institution but enforced across classes unequally. Those who believe honor cannot be restored through courts are treated as irrational rather than heard.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished and dueling norms re-legitimized, insurance markets would reprice or exit, legal remedies would compete with violent vindication, and state authority would lose a primary claim to monopoly over satisfaction of public injury. Military recruitment and advancement would shift to valorize dueling skill again. The entire legal and commercial apparatus built around honor-prohibition would require reorganization.
% FOUNDING_PROBLEM: Public insults to honor and reputation created feuds, escalated violence, and destabilized social order. Historical dueling systems provided a recognized, formalized outlet that could terminate disputes (one party killed or defeated, honor deemed satisfied). As state centralization progressed, the state monopolized the authority to define when disputes were resolved, and as bourgeois commercial norms spread, the very category of honor-through-violence became incompatible with rational economic trust.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and legal scholars attest the founding problem (honor-feuds, destabilization) is solved by prohibition and court jurisdiction. Military and aristocratic actors attest the founding problem (reputation damage from unredressed insult) persists and is inadequately addressed by courts. Cultural historians and insurance economists independently document the composite mechanism: state legal prohibition, bourgeois cultural reframing, insurance market exclusion, and category-shift (honor → psychology) each operated in parallel, and none alone would have succeeded without the others.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness value (0.68, plateau by 1880) reflects the constraint's operation as asymmetric extraction: the state gains monopoly authority, the bourgeoisie gain cultural hegemony, insurers gain financial compliance mechanisms — all while military aristocracy and identity-locked honor-seekers bear the costs. The suppression value (0.71) is correspondingly high because multiple enforcement channels operate in parallel: criminal law, insurance exclusion, social ostracism, and the cultural narrative that dueling is barbaric. Theater is moderate-high (0.54 by 1900) because a substantial portion of the constraint's persistence by 1900 consists of performed compliance — honor-satisfaction through courts is theatrically presented as adequate, while participants privately regard it as insufficient. The measurement series shows activation over the 150-year interval: extractiveness and suppression both rise rapidly from 1750–1860, then plateau by 1880, indicating that the composite mechanism reached its equilibrium force by the Industrial era. Theater rises more gradually, indicating that theatrical performance was a later addition to raw enforcement. The grid records this composite pressure on one time axis — no metric has its own measurement schedule.
 *
 * PERSPECTIVAL GAP:
 *   From the state and bourgeois seats, the constraint is understood as coordination (rational dispute-resolution replacing feudal violence), and they experience it as natural evolution toward civilization. From the aristocratic and military seats, the constraint operates as coordinated extraction (multiple institutional actors attacking the legitimacy of honor-vindication for state/bourgeois benefit), and those seats experience it as identity-destruction. The engine computes these divergent per-seat types from the structural data: beneficiary seats derive low d (subsidized by the constraint), target seats derive high d (burdened by extraction), and excluded/observer seats occupy intermediate positions. The authored claim (tangled_rope) asserts the asymmetric structure; the authored metrics (high extractiveness, high suppression) are consistent with that claim but do not derive it.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority is the primary beneficiary (gains monopoly, collects allegiance to courts) — d near 0. Bourgeois professional class collects cultural hegemony and professional legitimacy — d low (0.2–0.3). Insurance underwriters collect financial compliance and exclusion rents — d moderate (0.4). Military aristocracy are the primary targets: they lose identity-constitutive practices, face criminal liability, see their codes displaced by bourgeois norms — d very high (0.85+). Urban lower classes occupy an intermediate position: they never benefited from dueling access (d moderate-high on extraction), but are unequally targeted for enforcement compared to aristocrats (d tempered by enforcement gradient). Excluded duelists are near-pure targets (d 0.8+): they face all costs (prohibition, insurance, social ostracism) and collect no coordination benefit. The directionality derivation flows from beneficiary/victim declaration plus exit-option modulation: identity_locked status for aristocrats amplifies d toward full-target end because exit (abandoning honor-vindication) is constitutive-identity abandonment, not mere institutional exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy collapse because its founding problem (honor-feuds destabilizing order) remains live even as the satisfaction mechanism shifts. The state maintains a mandate to prevent feuding violence. The bourgeoisie maintain a mandate to advance rational norms. Insurers maintain a mandate to price risk. None of these mandates died; they evolved and synchronized. However, omega_founding_problem_status documents the contestation: military aristocrats and historians of aristocratic culture attest the founding problem (personal honor damage) persists unresolved, while state and bourgeois actors attest it is solved. This contested status routes through the mandatrophy mismatch detector (founding_problem_status=contested + disappearance_verdict=world_rearranges triggers review) and flags that the constraint's persistence is not mandated by universal problem-solving but by asymmetric institutional interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_independence_or_coupling,
    'Were the four mechanisms (state legal prohibition, bourgeois cultural norms, insurance exclusion, category-shift) truly independent causal forces, or were they structurally coupled such that any one would have triggered the others?',
    'Comparative historical analysis: examine cases where only one or two mechanisms operated (e.g., jurisdictions with state prohibition but weak bourgeois institutions, or regions where category-shift occurred without insurance market development) and ask whether dueling persisted longer or shorter than the composite mechanism predicts.',
    'If independent, the composite reading is correct — multiple institutional actors attacked honor-vindication simultaneously for distinct benefits. If tightly coupled, one mechanism was primary and the others followed; the constraint might reclassify as state monopoly (primary) with secondary effects rather than true composite operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_independence_or_coupling, empirical, 'Whether the four mechanisms were independent or causally coupled.').

omega_variable(
    category_shift_as_enforcement_or_discovery,
    'Did the category-shift (honor from objective social fact to subjective psychological state) represent a genuine epistemic discovery that honor-satisfaction through courts is actually adequate, or was it a discursive technology engineered by bourgeois institutions to delegitimize aristocratic claims?',
    'Textual genealogy: trace the emergence of psychological honor theories in legal doctrine and cultural discourse. Examine whether the shift was continuous with earlier philosophical traditions or a sharp break. Assess whether benefits accrued asymmetrically to bourgeois professionals and state authority.',
    'If discovery: the category-shift is conceptual and somewhat independent of institutional extraction; dueling declined because people realized courts work. If engineered: the category-shift is itself an extractive mechanism (redefining honor to advantage those who control courts), and the extraction is higher. The constraint''s type remains tangled_rope either way, but the omega documents ambiguity about whether the shift is epistemic advancement or institutional capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_shift_as_enforcement_or_discovery, conceptual, 'Whether the honor category-shift was epistemic discovery or discursive technology for institutional advantage.').

omega_variable(
    sibling_reading_identity,
    'How does the composite_reading (multiple mechanisms, asymmetric extraction) relate structurally to the contraction_reading (honor-vindication became cognitively unthinkable) and the decline_reading (dueling simply declined in frequency)?',
    'The three readings are not mutually exclusive — they answer different causal questions (HOW did prohibition succeed, WHAT made dueling unthinkable, WHY did frequency drop). A full historical account might incorporate all three. The engine routes this through omega because the three readings assign causal weight differently and each benefits different analytical traditions.',
    'If contraction_reading is correct, the cognitive impossibility is primary and the extractive mechanisms are secondary effects. If decline_reading is correct, preference shifts are primary and institutional enforcement is secondary. The composite_reading asserts institutional extraction is primary. Each reading yields different policy implications: contraction suggests the constraint persists because the alternative is culturally unintelligible; decline suggests it persists via preference stability; composite suggests it persists via institutional interest and extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_identity, conceptual, 'Structural relationship between this reading and its siblings in the kernel contest.').

omega_variable(
    military_identity_lock_degree,
    'Is the military aristocracy''s exit_options classification as identity_locked accurate, or does it overstate the degree to which military service and honor-vindication are inseparable for aristocratic actors?',
    'Biographical and institutional history: examine military officers and aristocrats who abandoned honor-vindication practices. Assess whether they experienced this as identity-loss or as pragmatic adaptation. Examine patterns of career continuation, family status, and psychological integration for those who accepted non-violent honor resolution.',
    'If truly identity-locked, the directionality is near-maximum (0.85+) and the constraint imposes identity-destruction costs. If partially constrained but not locked, d would be lower (0.65–0.75). The exit-option classification affects the directionality computation and thus the per-seat type calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_identity_lock_degree, empirical, 'Degree to which military-aristocratic identity is constitutively dependent on honor-vindication practices.').

omega_variable(
    lower_class_uneven_enforcement,
    'Did the prohibition on dueling apply evenly across social classes, or was it enforced asymmetrically (strictly on lower classes, leniently on aristocrats until later)?',
    'Prosecution records, criminal sentencing data, and historical accounts of actual enforcement from 1750–1900. Examine whether aristocrats and military officers faced the same legal consequences as lower-class participants in honor violence.',
    'If enforcement was uneven (which historical evidence suggests), the constraint functioned initially as a selective extraction mechanism targeting lower classes while aristocrats retained de facto dueling access. This would mean victims were initially misidentified and the constraint was more purely extractive (snare-like) in early period before becoming uniformly tangled_rope by 1880s. The measurement series and suppression values would then reflect a ratcheting of enforcement to target all classes equally over the interval.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lower_class_uneven_enforcement, empirical, 'Whether dueling prohibition was enforced evenly across social classes or asymmetrically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1750, 0.25).
narrative_ontology:measurement_basis(hono_tr_t1750, projected).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1800, 0.35).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1830, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1830, 0.45).
narrative_ontology:measurement_basis(hono_tr_t1830, observed).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1860, 0.52).
narrative_ontology:measurement_basis(hono_tr_t1860, observed).
narrative_ontology:measurement(hono_tr_t1880, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1880, 0.54).
narrative_ontology:measurement_basis(hono_tr_t1880, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.54).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1750, 0.31).
narrative_ontology:measurement_basis(hono_be_t1750, projected).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1800, 0.48).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1830, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1830, 0.61).
narrative_ontology:measurement_basis(hono_be_t1830, observed).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1860, 0.66).
narrative_ontology:measurement_basis(hono_be_t1860, observed).
narrative_ontology:measurement(hono_be_t1880, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1880, 0.68).
narrative_ontology:measurement_basis(hono_be_t1880, observed).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1750, 0.38).
narrative_ontology:measurement_basis(hono_su_t1750, projected).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1800, 0.52).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1830, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1830, 0.64).
narrative_ontology:measurement_basis(hono_su_t1830, observed).
narrative_ontology:measurement(hono_su_t1860, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1860, 0.7).
narrative_ontology:measurement_basis(hono_su_t1860, observed).
narrative_ontology:measurement(hono_su_t1880, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1880, 0.71).
narrative_ontology:measurement_basis(hono_su_t1880, observed).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement_basis(hono_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__composite_reading, 0.18).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, state_monopoly_over_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, bourgeois_normativity_legal_standard).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, insurance_market_exclusion_mechanism).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_mechanism kernel admits three readings, each instantiating a different constraint with different ε and structural interpretation. The composite_reading (this file) asserts multiple synchronized extractive mechanisms; the contraction_reading asserts cognitive category impossibility; the decline_reading asserts frequency decline via preference shift. All three read the same historical record but weight mechanisms differently. Link all three via network.affects_constraints to document that honor prohibition is not a single constraint but a family of competing causal explanations of one historical process. The engine's per-seat type computations will diverge across readings due to different beneficiary/victim/exit declarations, providing a diagnostic test of which reading better predicts institutional behavior.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__composite_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
