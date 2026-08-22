% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right to Firearms (Unconnected to Militia Service)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   The Second Amendment protects an individual right to own firearms
 *   unconnected to militia service. This reading, adopted by the Supreme
 *   Court in District of Columbia v. Heller (2008) and expanded in New York
 *   State Rifle & Pistol Association v. Bruen (2022), interprets the
 *   Amendment as granting individuals a prerogative to possess firearms for
 *   lawful purposes—self-defense, hunting, sport—without requirement of
 *   militia participation or service. The constraint operates by applying
 *   strict scrutiny to firearms regulations, making it difficult for states
 *   and cities to impose categorical restrictions, comprehensive licensing,
 *   or prohibition. This reading is one instantiation of a contested kernel
 *   (the meaning of the Second Amendment); sibling readings include the
 *   collective-militia reading (protecting state militia authority only) and
 *   the civic-right reading (protecting an individual right conditioned on
 *   civic participation). The constraint benefits individual firearms owners,
 *   advocacy organizations, and manufacturers while constraining state
 *   regulatory authority and public health constituencies. Claim and metrics
 *   are deliberately misaligned: the constraint is CLAIMED as tangled_rope
 *   (genuine coordination—clarifying a constitutional ambiguity—plus
 *   asymmetric extraction—constraining state regulatory authority); the
 *   metrics describe substantial extractiveness (0.68 at present) because the
 *   individual-right reading broadly benefits one constituency while imposing
 *   costs on another, with active legal enforcement required to maintain the
 *   interpretation's scope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.52).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right to Firearms (Unconnected to Militia Service)").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'e0f6fd87-c84e-4f39-bc19-829f921ff715').
narrative_ontology:cs_kernel_codification('e0f6fd87-c84e-4f39-bc19-829f921ff715', fixed_text).
narrative_ontology:cs_authority_grounding('e0f6fd87-c84e-4f39-bc19-829f921ff715', lineage).
narrative_ontology:cs_interpretation_layer_present('e0f6fd87-c84e-4f39-bc19-829f921ff715').
narrative_ontology:cs_reading_relation('e0f6fd87-c84e-4f39-bc19-829f921ff715', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('e0f6fd87-c84e-4f39-bc19-829f921ff715', second_amendment_scope__civic_right_reading, influences).
narrative_ontology:cs_axiom('e0f6fd87-c84e-4f39-bc19-829f921ff715', foundational, individual_prerogative_unconnected_to_militia).
narrative_ontology:cs_axiom_status(individual_prerogative_unconnected_to_militia, holdable).
narrative_ontology:cs_axiom_grounding('e0f6fd87-c84e-4f39-bc19-829f921ff715', individual_prerogative_unconnected_to_militia, empirically_contingent).
narrative_ontology:cs_axiom('e0f6fd87-c84e-4f39-bc19-829f921ff715', secondary, strict_scrutiny_firearms_regulation).
narrative_ontology:cs_axiom_status(strict_scrutiny_firearms_regulation, holdable).
narrative_ontology:cs_axiom_grounding('e0f6fd87-c84e-4f39-bc19-829f921ff715', strict_scrutiny_firearms_regulation, deontological).
narrative_ontology:cs_reference_frame('e0f6fd87-c84e-4f39-bc19-829f921ff715', original_public_meaning_individual_right).
narrative_ontology:cs_drift_state('e0f6fd87-c84e-4f39-bc19-829f921ff715', contemporary_post_bruen_2022, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e0f6fd87-c84e-4f39-bc19-829f921ff715', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, second_amendment_advocacy_organizations).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearm_manufacturers).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, cities_with_restrictions).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_health_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, constitutional_originalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals claiming the right to own firearms for self-defense, hunting, sport, and personal security without demonstrating militia service or membership. Under this reading, they are primary rights-holders; the constraint protects their prerogative against state restriction. Exit available through relocation to jurisdictions with less restrictive laws or through legal challenge.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_firearms_owners, beneficiary,
    moderate, biographical, mobile, national).

% Organizations mobilize legal and political resources to defend and expand individual firearms rights. They litigate cases, lobby legislatures, and shape public discourse around the constraint's interpretation. They benefit from the individual-right reading by legitimizing their advocacy frame and constraining state authority to regulate.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, second_amendment_advocacy_organizations, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, second_amendment_advocacy_organizations, agenda_setter).

% Manufactures and sells firearms to the civilian market. Benefits from the individual-right reading by maintaining broad access to a large customer base and constraining states' ability to impose design regulations, sales restrictions, or prohibitions that would narrow the market. Can relocate operations or distribute through less-regulated channels if state laws tighten.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearm_manufacturers, beneficiary,
    powerful, generational, arbitrage, national).

% State legislatures, executives, and agencies attempting to regulate firearms through licensing, background checks, waiting periods, and category restrictions (e.g., assault weapons). Under this reading, their regulatory authority is sharply constrained by strict scrutiny applied to firearms regulations. They bear the cost of defending restrictions against constitutional challenge and lose authority over a domain they historically controlled.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_regulatory_authority, payer,
    institutional, generational, constrained, national).

% Municipal governments with local firearms ordinances (e.g., handgun bans, storage requirements, licensing schemes) face preemption and legal challenge under the individual-right reading. They bear the cost of litigation, the loss of local regulatory discretion, and the constraint on their ability to tailor firearms policy to local conditions and demographics.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, cities_with_restrictions, payer,
    moderate, biographical, constrained, regional).

% Public health researchers, medical associations, and gun violence prevention advocacy groups bear the cost that restrictive regulations are struck down on constitutional grounds. Their ability to implement evidence-based harm-reduction measures (background checks, waiting periods, red-flag laws) is constrained by strict scrutiny. They experience the constraint as blocking policy levers they identify as necessary for public health.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_health_constituencies, payer,
    moderate, biographical, constrained, national).

% Sets the constitutional interpretation through binding precedent (District of Columbia v. Heller, 2008; New York State Rifle & Pistol Association v. Bruen, 2022). Determines the scope of the individual right and the level of scrutiny applied to regulations. Enforces the interpretation through doctrinal authority and reversal of lower-court decisions.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Legal scholars and jurists interpreting the Constitution through original public meaning at the time of ratification. This reading aligns with their methodological commitments: the original meaning of 'the right of the people to keep and bear Arms' includes an individual right unconnected to militia service (to the extent the founding-era sources support it). They benefit from the individual-right reading as it vindicates their interpretive method.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, constitutional_originalists, beneficiary,
    analytical, generational, analytical, universal).

% Legal scholars and advocates arguing for the collective-militia reading of the Second Amendment (that it protects state authority to maintain militias, not individual ownership rights). They are excluded from the agenda-setting role under the individual-right reading; their interpretation is judicially foreclosed by Heller and Bruen. They would argue for a different constitutional meaning but have no institutional seat in the constraint as currently enforced.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, collective_right_advocates, excluded,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, second_amendment_advocacy_organizations).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the constitutional meaning of 'the right of the people to keep and bear Arms' by specifying that it protects individuals' prerogative to possess firearms for lawful purposes (self-defense, hunting, sport) independent of militia service. This coordination function creates legal certainty about the scope of the right and the types of regulations that survive constitutional review.
% TRANSFER_FUNCTION: Transfers regulatory authority from state/local governments to individuals: individuals gain the prerogative to possess firearms subject only to narrow, constitutionally justified restrictions; states lose the authority to impose broad categorical restrictions, comprehensive licensing regimes, or prohibition without satisfying strict scrutiny. The flow of authority is from public bodies to private rights-holders.
% ABSENT_VOICES: Collective-right advocates and civic-right advocates (those reading the Second Amendment as protecting a right conditioned on militia participation or as protecting only state authority, not individual ownership) are structurally excluded from the agenda-setting table. They would contest the individual-right reading's premise but are judicially foreclosed by binding precedent. Voices emphasizing public health constraints on the right are also marginalized: the constraint's strict scrutiny standard makes it difficult for them to prevail in court regardless of empirical evidence.
% DISAPPEARANCE_RATIONALE: If this constitutional reading were suddenly reversed or abandoned, state and local governments would immediately reassert regulatory authority over firearms (licensing, registration, category restrictions, prohibition). The firearms industry would face significant new market constraints. Individual firearms owners would lose doctrinal protection against regulations they currently challenge successfully. The political and legal landscape around firearms regulation would shift dramatically toward public health and regulatory approaches currently blocked by strict scrutiny.
% FOUNDING_PROBLEM: What does the Second Amendment mean? Specifically: does it protect an individual right to possess firearms independent of militia service, or does it protect only the state's authority to maintain militias, or does it protect an individual right conditioned on participation in civic militia structures?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live and contested: the Supreme Court has ruled (Heller, 2008) that the individual-right reading is correct, but lower courts, legislatures, and legal scholars continue to dispute the scope of the right and the appropriate level of constitutional scrutiny. Scholars outside the benefiting parties (originalist jurists, legal historians) corroborate that the founding-era sources are genuinely ambiguous on this question and that the individual-right reading is a defensible but contested interpretation of the historical record.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the individual-right reading creates a broad beneficiary class (all individuals) and concentrates costs on state regulatory authority and public health constituencies. The reading's ε is not determined by whether the interpretation is 'correct' in some absolute sense; it is determined by the structural asymmetry it creates: one group (individuals, manufacturers, advocacy organizations) gains a prerogative that was previously unclear or denied, while another group (state governments, public health bodies) loses authority they previously wielded. Suppression is moderate (0.52) because the constraint's persistence requires active judicial enforcement (strict scrutiny review of regulations) and political mobilization (advocacy organizations defending the reading against challenges and reinterpretation). Theater ratio is low-moderate (0.28): the security review function of the First Amendment is real (scrutiny is designed to separate legitimate from illegitimate restrictions), but a growing share of enforcement activity defends the breadth of the individual right against regulations that would otherwise be justified by public health or harm-reduction evidence. The temporal series models the historical arc: the individual-right reading was marginal (low extractiveness) before the Civil Rights era, gained force during the late 20th century (rising extractiveness), and crystallized into binding precedent with Heller (2008) and especially Bruen (2022), after which extractiveness plateaued. The measurement grid runs on a shared time axis (1791, 1900, 1970, 2008, 2022, 2026) so all metrics are authored at every point. Early values are projected; post-1900 values are observed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Supreme Court, originalist scholars, constitutional originalists) and the beneficiary seat (individual firearms owners, advocacy organizations) should compute the constraint as legitimate coordination—clarifying an ambiguous constitutional text. The payer seat (state regulatory authority, public health constituencies) should compute the constraint as extraction—losing authority to regulate in a domain where they can point to empirical harms (gun violence, mass shooting prevention) and democratic mandates for regulation. The engine computes this divergence from the structural data (beneficiary/victim, exit options, power). The high accessibility collapse (0.71) reflects that once the individual-right interpretation crystallizes into Supreme Court precedent, the alternatives (collective-militia, civic-right) become difficult for states and localities to adopt without federal constitutional amendment. Resistance is high (0.74) because public health bodies, gun violence prevention organizations, and many state/local officials actively contest the individual-right reading's scope and attempt to find regulations that survive strict scrutiny (e.g., age restrictions, background checks). The combination of high accessibility collapse and high resistance indicates a constraint where alternatives are constrained by law and doctrine but politically contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual firearms owners sit at high directionality (d near 1.0, full target—wait, that's wrong; let me reconsider). Actually: Individual firearms owners are BENEFICIARIES, so d should be LOW (near 0.0, full beneficiary). They gain prerogatives under this reading. States lose authority, so they are PAYERS, and d is HIGH (near 1.0, full target). Second Amendment advocacy organizations are BENEFICIARIES (d low). Firearm manufacturers are BENEFICIARIES (d low). Public health constituencies are PAYERS (d high). This creates a substantial directionality spread: beneficiaries experience the constraint as protective (low d, subsidy-like in that it protects rather than extracts), while payers experience it as constraining and extractive (high d). The asymmetry is structural: the individual-right reading unilaterally shifts authority. No directionality override is needed; the structural derivation from beneficiary/victim + exit options + power captures the relationship accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (what does the Second Amendment mean?) remains live: legal scholars continue to dispute whether the historical record supports the individual-right reading, and political actors continue to advocate for alternative interpretations. The constraint is NOT mandatrophic. However, mandatrophy pressure exists: if the empirical evidence on gun violence and regulation efficacy continues to accumulate, and if the individual-right reading is seen as blocking evidence-based policy, there will be increased pressure to adopt the civic-right or collective-militia reading as an alternative that would restore state authority. The current state of the constraint prevents that pressure from translating into doctrinal change, because strict-scrutiny judicial review makes it difficult to overturn the individual-right reading without explicit Supreme Court reversal or constitutional amendment. The theater ratio (0.28, low-moderate) suggests that the constraint is not yet performative (not a piton), but the gap between the founding problem (constitutional meaning) and the effect (constrained regulatory authority regardless of empirical evidence) creates a vulnerability to the argument that the constraint's function has shifted from coordination (clarifying the Constitution) to extraction (blocking evidence-based policy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_ambiguity,
    'What was the original public meaning of ''the right of the people to keep and bear Arms'' at ratification in 1791? Did it include an individual right to possess firearms for personal use unconnected to militia service?',
    'Historical scholarship on founding-era sources, state constitutions, militia practice, and common-law traditions. Textual analysis of period documents. Cross-jurisdictional comparison with other constitutional rights claims.',
    'The originalist axiom of the individual-right reading rests on the claim that the historical record supports an individual right. If robust historical evidence demonstrated that the founding meaning was militia-only or civic-conditional, the originalist case for the individual-right reading would collapse, and the reading would need to rest on a living-constitution or purposivist ground (which would undermine the axiom and allow the civic-right or collective-right reading to compete on equal doctrinal footing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_public_meaning_ambiguity, empirical, 'Whether historical evidence supports the individual-right reading''s claim to originalist grounding.').

omega_variable(
    militia_clause_relationship,
    'What is the logical and grammatical relationship between the prefatory militia clause and the operative right-to-bear-arms clause? Does the militia clause condition the operative clause (making the right contingent on militia service) or merely provide context (leaving the operative clause to stand independent)?',
    'Grammatical and structural linguistic analysis. Comparison with other constitutional constructions (e.g., other prefatory clauses in founding-era documents). Originalist and living-constitution jurisprudence.',
    'The individual-right reading treats the militia clause as prefatory context, not as a condition. The civic-right reading treats the militia clause as partially conditioning the right. If linguistic analysis established that the militia clause is genuinely conditional (not merely prefatory), the individual-right reading would weaken, and the civic-right reading would gain doctrinal force. This is not a minor textual question; it is the grammatical hinge the three readings turn on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_relationship, empirical, 'Whether the prefatory clause conditions or merely contextualizes the operative clause.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the individual-right reading''s primary function to resolve a genuine constitutional ambiguity (coordination function), or to reallocate authority from collective bodies to individuals (extraction function)?',
    'Post-Heller jurisprudential and policy analysis: if the individual-right reading is purely coordinative, we should observe state and local governments accepting the clarification and redesigning regulations to fit the new boundaries without systematic resistance. If it is primarily extractive, we should observe persistent political pressure to reinterpret or amend the Constitution, and systematic legal challenges to the reading''s scope. We should also observe whether the beneficiaries of the reading express satisfaction with the coordination or demand further expansion of the right.',
    'If the reading is purely coordinative, it is a rope; if primarily extractive, it is a snare (or tangled_rope with strong asymmetry). The engine computes this from the structural data (beneficiary/victim, extraction metrics), but the political trajectory will clarify the reading''s true function: a reading that clarifies the Constitution should stabilize dispute; a reading that extracts authority should fuel continued contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether the individual-right reading is primarily coordination or extraction.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the individual-right reading and the collective-right reading foreclose each other (such that no single constitutional framework can hold both), or do they coexist as live policy positions held by different factions?',
    'Constitutional law analysis: if the readings are mutually exclusive at the textual level (both cannot be true of the same text), they foreclose. If the readings differ on policy outcomes (scope of regulation) while accepting a shared textual foundation, they coexist. Examine whether courts, legislatures, and scholars treat the readings as binary choices or as points on a continuum.',
    'If they foreclose, the current dominance of the individual-right reading (via Heller/Bruen precedent) eliminates the collective-right reading as a constitutionally available option. If they coexist, the readings remain live alternatives at the level of political contestation, and reinterpretation is possible. This affects the constraint''s stability and its vulnerability to legal reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether alternative readings of the Second Amendment''s scope are logically foreclosed or coexistent.').

omega_variable(
    strict_scrutiny_gatekeeping,
    'Is strict scrutiny applied to Second Amendment regulations the correct level of review for this constitutional right, or should intermediate or rational-basis scrutiny apply?',
    'Constitutional doctrine evolution. Compare the level of scrutiny applied to other constitutional rights (First Amendment speech, Fourth Amendment searches, Fourteenth Amendment equal protection) and examine whether the same doctrinal rationale supports strict scrutiny for the Second Amendment. Examine whether alternative levels of scrutiny would change which regulations survive review.',
    'Strict scrutiny makes it difficult for regulations to survive (they must be narrowly tailored to a compelling state interest). Intermediate scrutiny (the level applied to gender discrimination and commercial speech) is more permissive. Rational-basis scrutiny (the minimal level) would allow almost any regulation to survive. If the appropriate level of scrutiny is lower than strict, state regulatory authority would expand substantially, and the individual-right reading''s extractive effect would diminish. This is the mechanism by which future constitutional interpretation could weaken (without overruling Heller/Bruen precedent) the constraint''s current scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_scrutiny_gatekeeping, conceptual, 'Whether strict scrutiny is the correct level of constitutional review for Second Amendment regulations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__individual_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement_basis(seco_tr_t1791, projected).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_scope__individual_right_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1970, second_amendment_scope__individual_right_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement_basis(seco_tr_t1970, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.26).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_scope__individual_right_reading, theater_ratio, 2022, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_scope__individual_right_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__individual_right_reading, base_extractiveness, 1791, 0.35).
narrative_ontology:measurement_basis(seco_be_t1791, projected).
narrative_ontology:measurement(seco_be_t1900, second_amendment_scope__individual_right_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1970, second_amendment_scope__individual_right_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement_basis(seco_be_t1970, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_scope__individual_right_reading, base_extractiveness, 2022, 0.68).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_scope__individual_right_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(seco_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__individual_right_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement_basis(seco_su_t1791, projected).
narrative_ontology:measurement(seco_su_t1900, second_amendment_scope__individual_right_reading, suppression_requirement, 1900, 0.25).
narrative_ontology:measurement_basis(seco_su_t1900, observed).
narrative_ontology:measurement(seco_su_t1970, second_amendment_scope__individual_right_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement_basis(seco_su_t1970, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_scope__individual_right_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2026, second_amendment_scope__individual_right_reading, suppression_requirement, 2026, 0.52).
narrative_ontology:measurement_basis(seco_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_scope kernel decomposes into three readings, each instantiating a distinct constraint with different beneficiary/victim structures, ε values, and directionality profiles. The individual_right_reading benefits individuals and constrains state authority (ε=0.68, high extractiveness due to broad beneficiary coverage and asymmetric authority shift). The collective_right_reading benefits state militia authority and constrains individual prerogative (inverse ε and directionality). The civic_right_reading occupies a middle position, conditioning individual rights on civic participation. These three constraints are related by the kernel they interpret but remain structurally distinct. The individual-right reading currently dominates binding precedent (Heller 2008, Bruen 2022), making the alternative readings judicially foreclosed but politically contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__individual_right_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
