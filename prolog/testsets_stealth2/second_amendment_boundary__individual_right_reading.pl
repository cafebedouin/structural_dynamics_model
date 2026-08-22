% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Individual-Right Boundary (Individual-Right Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   A constitutional-interpretive boundary instantiated by the
 *   individual-right reading of the Second Amendment: the operative clause is
 *   held to establish a pre-existing individual entitlement, while the
 *   prefatory militia clause announces purpose without bounding scope. Across
 *   the interval the reading has moved private possession into a protected
 *   domain, recast state regulation as presumptive infringement, and
 *   progressively shielded the firearms market from democratic adjustment.
 *   Per the committer frame this file is ONE READING of the contested
 *   second_amendment_boundary kernel: the sibling readings are separate
 *   constraints with their own epsilon, beneficiaries, and victims, linked by
 *   network edges rather than folded into this file. KEY AGENTS (by
 *   structural relationship): - scotus_individual_right_majority:
 *   Agenda-setter (institutional/constrained) — authors and administers the
 *   reading; collects interpretive authority - private_firearm_owners:
 *   Primary beneficiary (organized/identity_locked) — possession placed
 *   beyond ordinary regulatory reach; identity fused with the entitlement -
 *   firearms_industry: Primary beneficiary (powerful/arbitrage) — market
 *   demand constitutionally floored; captures the monetary gains -
 *   mass_shooting_victims_survivors: Payer (powerless/trapped) — bear the
 *   safety cost of shielded access - domestic_violence_victims: Payer
 *   (powerless/trapped) — face abuser firearm access the framework regulates
 *   reluctantly - firearm_suicide_bereaved_families: Payer
 *   (powerless/trapped) — means-restriction interventions blunted -
 *   state_regulatory_authorities: Payer (institutional/constrained) —
 *   regulatory authority foreclosed - urban_gun_violence_communities:
 *   Excluded (powerless/trapped) — bear concentrated burden with no doctrinal
 *   seat - second_amendment_historians: Observer (analytical/analytical) —
 *   supply the archive the method consumes
 *
 * KEY AGENTS:
 *   - scotus_individual_right_majority: agenda-setter (institutional/constrained) — administers the reading and the test that decides which regulations survive
 *   - private_firearm_owners: primary beneficiary (organized/identity_locked) — possession protected; exit experienced as self-erasure
 *   - firearms_industry: primary beneficiary (powerful/arbitrage) — demand floor and litigation funding; the seat the monetary gains accrue to
 *   - mass_shooting_victims_survivors: payer (powerless/trapped) — bear the mortality cost of wide lawful access
 *   - domestic_violence_victims: payer (powerless/trapped) — bear elevated intimate-partner lethality
 *   - firearm_suicide_bereaved_families: payer (powerless/trapped) — bear the suicide-completion cost of continuous home access
 *   - state_regulatory_authorities: payer (institutional/constrained) — lost final authority over the subject
 *   - urban_gun_violence_communities: excluded (powerless/trapped) — concentrated burden, no seat in the interpretive forum
 *   - second_amendment_historians: observer (analytical/analytical) — archive suppliers and method critics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.62).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.74).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual-Right Boundary (Individual-Right Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, 'afe0965b-74ea-44ff-9363-48fc74ab3a5a').
narrative_ontology:cs_kernel_codification('afe0965b-74ea-44ff-9363-48fc74ab3a5a', fixed_text).
narrative_ontology:cs_authority_grounding('afe0965b-74ea-44ff-9363-48fc74ab3a5a', lineage).
narrative_ontology:cs_interpretation_layer_present('afe0965b-74ea-44ff-9363-48fc74ab3a5a').
narrative_ontology:cs_reading_relation('afe0965b-74ea-44ff-9363-48fc74ab3a5a', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('afe0965b-74ea-44ff-9363-48fc74ab3a5a', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('afe0965b-74ea-44ff-9363-48fc74ab3a5a', foundational, operative_clause_independent_individual_right).
narrative_ontology:cs_axiom_status(operative_clause_independent_individual_right, holdable).
narrative_ontology:cs_axiom_grounding('afe0965b-74ea-44ff-9363-48fc74ab3a5a', operative_clause_independent_individual_right, deontological).
narrative_ontology:cs_axiom('afe0965b-74ea-44ff-9363-48fc74ab3a5a', secondary, prefatory_clause_announces_purpose_not_limit).
narrative_ontology:cs_axiom_status(prefatory_clause_announces_purpose_not_limit, holdable).
narrative_ontology:cs_axiom_grounding('afe0965b-74ea-44ff-9363-48fc74ab3a5a', prefatory_clause_announces_purpose_not_limit, conventional).
narrative_ontology:cs_reference_frame('afe0965b-74ea-44ff-9363-48fc74ab3a5a', preexisting_individual_arms_liberty).
narrative_ontology:cs_drift_state('afe0965b-74ea-44ff-9363-48fc74ab3a5a', post_bruen_contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('afe0965b-74ea-44ff-9363-48fc74ab3a5a', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, private_firearm_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims_survivors).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, firearm_suicide_bereaved_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, private_firearm_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_regulatory_authorities).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, heller_individual_right_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, history_and_tradition_canon).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, preexisting_rights_originalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sitting majority that adopted the reading in 2008 and extended it in subsequent terms, administering the history-and-tradition test that decides which firearms regulations survive. It collects interpretive authority from serving as the text's custodian and pays a legitimacy cost with each contested extension. Institutional commitment and precedent bind it; exit means reversing its own handiwork at reputational expense.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, scotus_individual_right_majority, agenda_setter,
    institutional, generational, constrained, national).

% Own firearms for self-defense, sport, and inheritance; the reading places that possession beyond ordinary regulatory reach. Political organizations aggregate their preferences, and the entitlement has fused with civic identity for a large share of the constituency, so relinquishment is experienced as self-erasure rather than a policy trade. Owner households also bear a share of the harm burden, since firearms in the home are the dominant method of suicide. Exit means divesting possessions and leaving a community defined by them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, private_firearm_owners, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, private_firearm_owners, payer).

% Manufacturers, retailers, and their trade associations sell into a market whose demand floor is set by the entitlement: measures that would shrink the customer base, such as assault-weapon bans, licensing, and waiting periods, face a presumption of invalidity. Revenue tracks the protected market, and the trade association funds the litigation that maintains it. Capital could move to other product lines or jurisdictions, which is precisely why the industry invests in keeping the market itself intact.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_industry, beneficiary,
    powerful, generational, arbitrage, national).

% Killed or wounded in attacks carried out with lawfully acquired firearms, or surviving those deaths and injuries. They bear the safety cost of widely available access without any seat in the interpretive process that shapes it; their recourse is tort litigation, which the surrounding legal architecture narrows. Exit is not available to the dead; survivors exit by private grief or by joining advocacy.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims_survivors, payer,
    powerless, immediate, trapped, national).

% Live with or flee partners who own firearms; firearm presence multiplies lethality in intimate-partner violence. The measures that most directly reduce their risk, such as surrender requirements and prohibition categories, are the class of regulation the reading treats with suspicion, and enforcement gaps persist even where rules survive. Leaving the relationship does not reliably leave the danger behind.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, national).

% Have lost members to suicide completed with firearms, the most lethal commonly available method. Temporary means-restriction during crisis periods is the intervention their losses point toward; the breadth of the entitlement keeps lethal means continuously present in homes. Families organize as advocates after the fact; the loss itself is unrecoverable.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_suicide_bereaved_families, payer,
    powerless, biographical, trapped, national).

% Legislatures and agencies that policed firearms under the former militia-confined understanding and now draft within a shrinking space, pre-clearing bills against litigation they expect to lose. They retain budgetary and administrative capacity but not final authority over the subject matter. Exit is unavailable; federal supremacy holds regardless of state preference.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_regulatory_authorities, payer,
    institutional, generational, constrained, regional).

% Neighborhoods carrying a concentrated share of firearm homicide, disproportionately asking for comprehensive licensing and removal regimes. They are absent from the interpretive forum: the governing method consults ratification-era materials rather than contemporary residents, and their preferred remedies sit outside what the current framework tolerates. Exit means migration, which reproduces the exposure elsewhere.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, urban_gun_violence_communities, excluded,
    powerless, biographical, trapped, local).

% Academic specialists whose archival work supplies, and contests, the founding-era record the governing test consumes. They hold no decision power; their filings shape outcomes indirectly, and their documented professional disagreement with the method's selectivity is on the record. Exit is professional: they can decline participation, at the cost of ceding the archive to partisan use.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, second_amendment_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Delimits state police power against a uniform national floor for private arms possession: residents of every jurisdiction hold the same minimum entitlement, resolved once by the interpreting court rather than separately by fifty legislatures, and owners and sellers gain stable expectations about what regulation can reach.
% TRANSFER_FUNCTION: Moves regulatory authority from state legislatures to the individual entitlement and its judicial custodian; moves the costs of widespread firearm availability, including homicide, intimate-partner lethality, and suicide completion, onto the victim classes; moves market stability to sellers and civic-status affirmation to owners.
% ABSENT_VOICES: The three victim classes hold no seat in the interpretive conversation: the dead cannot testify, survivors appear only as discrete litigants, and the history-and-tradition inquiry consults ratification-era materials rather than contemporary cost-bearers. Urban communities carrying concentrated homicide burden are absent from the doctrinal forum, and their preferred remedies sit outside what the framework tolerates. Holders of the militia-conditioned understanding were removed from the federal framework by the 2008 framework choice.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight and the reading were reversed, state legislatures would resume comprehensive regulation within legislative sessions, carry regimes would contract, the litigation economy built on history-and-tradition challenges would dissolve, and the firearms market would lose its constitutional demand floor; ownership patterns and manufacturer product lines would rearrange around restored regulatory space.
% FOUNDING_PROBLEM: The reading was constructed to solve the perceived vulnerability of private arms possession to majoritarian regulation: before 2008, federal appellate doctrine treated the amendment as militia-confined and upheld comprehensive regulation, and the individual-right reading answered by locating possession beyond regulatory reach.
% FOUNDING_PROBLEM_CORROBORATION: Inside the benefiting coalition and the agenda-setting Court, the protective problem is attested as live. Outside it, corroboration is weak-to-adverse: founding-era historiography corroborates the original 1791 concern with standing armies and militia control but not the modern protective problem; public-health and gun-violence-prevention scholarship attests that the arrangement's costs now dominate its protective yield; no source outside the gun-rights coalition attests that the problem this arrangement solves remains live. That absence is itself signal.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope from structural analysis: the arrangement possesses a genuine coordination function (a uniform national floor, resolution of textual ambiguity, protection of a minority-held practice from majoritarian suppression) AND asymmetric extraction riding on it (externalized mortality and morbidity costs borne by three identifiable classes, a market whose demand floor is constitutionally set, and regulatory authority transferred from legislatures to a judicial custodian), held in place by active enforcement. Extractiveness is authored at 0.62: substantial, but discounted for the real protective yield millions of owners sincerely consume. Suppression is authored at 0.74 as a raw structural property, unscaled by power or scope: persistence depends on courts actively invalidating the rival framework and striking state experimentation, not on participant preference. Theater_ratio 0.40 reflects real doctrinal work mixed with a growing performative share, as the history-and-tradition method increasingly operates as a selectively curated archive reaching predetermined outcomes, a critique the historian community has put on the record. Accessibility_collapse 0.55: the rival reading is foreclosed inside the federal judiciary, but political and amendment routes remain open, so alternatives are narrowed rather than eliminated. Resistance 0.72: continuous litigation, legislative counter-effort, and scholarly opposition. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the suppression_requirement series is included because the story specifically tracks enforcement intensification, as the machinery for screening regulation matured and hardened across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different arrangements from the same twenty-seven words. From the agenda-setter seat the structure is legitimate self-government of a fixed text, and each extension is fidelity. From the owner seat it is protection of a pre-political liberty, with the identity fusion making the stakes existential rather than instrumental. From the industry seat it is a stable demand environment worth funding indefinitely. From the three payer seats the same structure is enforced exposure: a rule that forecloses the remedies their losses point to, administered by a forum they cannot reach. From the excluded seat it is voicelessness dressed as neutral method. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the owner and industry seats toward the beneficiary end of directionality, with the industry seat nearest the subsidy pole because its arbitrage-grade exit means it bears none of the arrangement's costs while collecting its monetary gains. The three victim classes derive high directionality, amplified by trapped exit: they cannot leave the exposure the arrangement maintains. The agenda-setter derives a mild beneficiary tilt, since it collects interpretive authority from custodianship while paying legitimacy costs. State authorities derive moderately high directionality as institutional cost-bearers with no exit. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms already separate the seats correctly, and the two institutional actors (the Court and the states) are differentiated by role declaration rather than by an override, which would wrongly hit both since they share the institutional power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two symmetrical mislabels. Mislabeling the arrangement as pure extraction would erase its genuine coordination function, the uniform floor and minority-liberty protection that millions sincerely consume; the tangled_rope claim keeps that function visible alongside the extraction. Accepting the reading's own naturality rhetoric, the pre-existing right presented as a discovered feature of the political order rather than a 2008 judicial construction, would push toward mountain treatment and immunize the extraction; the preexistence_naturality omega keeps that question open and marks the story as a false-summit candidate should the construction reading prevail. On lifecycle drift: the founding problem is contested rather than dead, so no zombie flag is asserted, but the trajectory is watched, since if the protective function fully atrophied while the boundary persisted as curated-archive performance, the arrangement would drift piton-ward with the theater_ratio series already trending upward. Scaffold is inapplicable: the arrangement carries no sunset and its justification is steady-state protection, not transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the second_amendment_boundary kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Adoption of the militia_conditioned_reading would remove private possession from the protected domain, restore comprehensive regulatory space, and dissolve this reading''s victim set into regulated subjects; adoption of the insurrectionist_reading would re-instrumentalize possession toward anti-government capacity and add a political-resistance justification this reading lacks. The disagreement is located in the scope-binding force of the prefatory clause.',
    'Sibling adoption changes the victim set, the beneficiary set, and epsilon discontinuously; the three readings are three constraints, not three opinions about one, and cross-reading epsilon comparison is meaningful only because each file fixes its own referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: reading-of-kernel position, sibling deltas, and the location of the interpretive dispute.').

omega_variable(
    preexistence_naturality,
    'Is the individual right genuinely pre-existing, a natural-law-like limit discoverable in founding-era practice, or a 21st-century judicial construction retroactively naturalized by originalist method?',
    'Founding-era historiography at scale: militia statutes, carry regulation, and founding-era arms regulation records as assembled in the post-2004 historians'' intervention and contested in the Bruen-era archive disputes.',
    'If constructed, the constraint is a false-summit candidate, a beneficiary-bearing construct wearing naturality; the naturality claim collapses into an interest-group and jurisprudential achievement, and the extraction profile weighs more heavily in classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preexistence_naturality, empirical, 'Whether the claimed pre-existing right is discovered or constructed.').

omega_variable(
    third_party_cost_attribution,
    'Are the mortality and morbidity costs borne by the victim classes attributable to the constitutional boundary itself, or to criminal misuse and untreated crisis that the boundary neither causes nor protects?',
    'Comparative analysis across jurisdictions holding armament roughly constant while regulatory stringency varies, plus natural experiments from post-Bruen carry expansion and from state-level policy discontinuities.',
    'If attribution fails, epsilon falls toward coordination-cost levels and the type drifts rope-ward; if it holds, the live question becomes the tangled_rope versus snare boundary rather than the presence of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_cost_attribution, empirical, 'Causal attribution of the victim classes'' costs to the boundary versus to misuse.').

omega_variable(
    owner_identity_lock_depth,
    'How much of the owner constituency''s attachment is identity fusion, where exit is unthinkable, versus instrumental security dependence, where exit is costly but available?',
    'Longitudinal ownership-motivation surveys and defection data where regulation, scandal, or demographic turnover shifts coalition membership.',
    'Deep identity lock entrenches the beneficiary seat and raises the arrangement''s persistence independent of its functional performance; instrumental dependence predicts faster rearrangement if the boundary falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(owner_identity_lock_depth, empirical, 'Depth of identity lock in the owner constituency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__individual_right_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(seco_tr_t3, second_amendment_boundary__individual_right_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(seco_tr_t6, second_amendment_boundary__individual_right_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(seco_tr_t9, second_amendment_boundary__individual_right_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(seco_tr_t12, second_amendment_boundary__individual_right_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(seco_tr_t15, second_amendment_boundary__individual_right_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(seco_tr_t18, second_amendment_boundary__individual_right_reading, theater_ratio, 18, 0.4).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__individual_right_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(seco_be_t3, second_amendment_boundary__individual_right_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(seco_be_t6, second_amendment_boundary__individual_right_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(seco_be_t9, second_amendment_boundary__individual_right_reading, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(seco_be_t12, second_amendment_boundary__individual_right_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(seco_be_t15, second_amendment_boundary__individual_right_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(seco_be_t18, second_amendment_boundary__individual_right_reading, base_extractiveness, 18, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__individual_right_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(seco_su_t3, second_amendment_boundary__individual_right_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(seco_su_t6, second_amendment_boundary__individual_right_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(seco_su_t9, second_amendment_boundary__individual_right_reading, suppression_requirement, 9, 0.65).
narrative_ontology:measurement(seco_su_t12, second_amendment_boundary__individual_right_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(seco_su_t15, second_amendment_boundary__individual_right_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(seco_su_t18, second_amendment_boundary__individual_right_reading, suppression_requirement, 18, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Second Amendment' conflates three structurally distinct constraints, decomposed per the epsilon-invariance principle: this individual-right reading (private possession in the protected domain; regulation presumptively infringing; victim set = those harmed by unrestricted access), the militia_conditioned_reading (scope bounded by militia purpose; regulatory space preserved; different victim and beneficiary sets), and the insurrectionist_reading (possession instrumental to anti-tyranny capacity). Each is a separate file with its own epsilon, beneficiaries, and victims; this file links both siblings via affects_constraints. Direction of influence: the individual-right reading currently dominates the judicial framework and therefore sets the structural conditions under which the siblings could be adopted, making it upstream of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
