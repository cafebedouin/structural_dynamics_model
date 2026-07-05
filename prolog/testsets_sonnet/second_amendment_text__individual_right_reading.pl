% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment — Individual Right Reading (Personal Self-Defense Independent of Militia Service)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the individual-right reading of the Second
 *   Amendment kernel: the operative clause ('the right of the people to keep
 *   and bear arms shall not be infringed') is read as securing a personal
 *   entitlement to possess and carry firearms for self-defense that does not
 *   depend on militia service or organized civic-defense participation. This
 *   reading gained doctrinal dominance through a line of Supreme Court
 *   decisions establishing an individual right and later a
 *   text-history-tradition methodology for evaluating regulations, displacing
 *   decades of lower-court consensus that had treated the right as
 *   militia-conditioned. The reading's structural delta from its siblings is
 *   sharp: it names individual gun owners, the firearms industry, and
 *   gun-rights advocacy organizations as concentrated beneficiaries, and it
 *   produces an identifiable victim set — domestic violence survivors whose
 *   abusers' disarmament becomes constitutionally contestable, communities
 *   bearing firearm mortality, and law enforcement officers operating in a
 *   legal environment of expanded lawful carriage. This is one of three
 *   sibling readings of the same kernel text; the collective_security_reading
 *   and originalist_civic_virtue_reading are separate constraint stories with
 *   their own ε values, not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.42).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.35).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment — Individual Right Reading (Personal Self-Defense Independent of Militia Service)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, 'c3709d55-53c2-4150-aa10-972f245254db').
narrative_ontology:cs_kernel_codification('c3709d55-53c2-4150-aa10-972f245254db', fixed_text).
narrative_ontology:cs_authority_grounding('c3709d55-53c2-4150-aa10-972f245254db', lineage).
narrative_ontology:cs_interpretation_layer_present('c3709d55-53c2-4150-aa10-972f245254db').
narrative_ontology:cs_reading_relation('c3709d55-53c2-4150-aa10-972f245254db', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('c3709d55-53c2-4150-aa10-972f245254db', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('c3709d55-53c2-4150-aa10-972f245254db', foundational, self_defense_right_independent_of_civic_role).
narrative_ontology:cs_axiom_status(self_defense_right_independent_of_civic_role, holdable).
narrative_ontology:cs_axiom_grounding('c3709d55-53c2-4150-aa10-972f245254db', self_defense_right_independent_of_civic_role, deontological).
narrative_ontology:cs_axiom('c3709d55-53c2-4150-aa10-972f245254db', secondary, prefatory_clause_non_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_non_limiting, holdable).
narrative_ontology:cs_axiom_grounding('c3709d55-53c2-4150-aa10-972f245254db', prefatory_clause_non_limiting, conventional).
narrative_ontology:cs_reference_frame('c3709d55-53c2-4150-aa10-972f245254db', individual_liberty_founding_understanding).
narrative_ontology:cs_drift_state('c3709d55-53c2-4150-aa10-972f245254db', post_heller_mcdonald_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c3709d55-53c2-4150-aa10-972f245254db', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, domestic_violence_survivors).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, communities_with_high_firearm_mortality).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, law_enforcement_officers_facing_armed_encounters).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, disarmament_ineligible_populations_seeking_restoration).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, natural_right_to_self_defense_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, individual_rights_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquire, carry, and use firearms for self-defense, hunting, and other lawful purposes without needing to demonstrate militia affiliation or organized service. Courts applying this reading strike down licensing schemes that require them to show special need. Exit is not really at issue for this seat — the reading expands rather than constrains their options.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Manufactures and sells firearms and ammunition into a market whose size and shape is substantially determined by how broadly the individual right is read. Funds litigation and lobbying to sustain and extend the individual-right interpretation, and benefits from statutory liability shields premised on the same constitutional framing. Faces essentially no exit cost from the constraint — it is a market participant benefiting from the rule, not a party bound by it.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_industry, beneficiary,
    organized, generational, arbitrage, national).

% Litigate test cases, draft model legislation, and mobilize members to entrench the individual-right reading in case law and statute. Sets the interpretive agenda for lower courts and legislatures by supplying the doctrinal architecture (test cases, amicus networks, model statutes) that operationalizes the reading. Collects membership dues, political influence, and policy wins from the reading's persistence.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations, beneficiary).

% Face elevated lethality risk when an abuser has access to a firearm; the individual-right reading has been invoked to challenge federal and state disarmament provisions for domestic-violence respondents (including as-applied challenges following broad individual-rights precedent). Cannot exit the risk structurally — their safety depends on enforcement mechanisms the reading makes harder to sustain against constitutional challenge.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, domestic_violence_survivors, payer,
    powerless, immediate, trapped, local).

% Bear disproportionate homicide, suicide, and accidental-death burden correlated with firearm density and permissive carry regimes that the individual-right reading has helped establish and defend against regulation. Residents cannot individually opt out of ambient community firearm prevalence; exit would mean relocation, which is not available to most.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, communities_with_high_firearm_mortality, payer,
    powerless, biographical, trapped, local).

% Encounter a civilian population in which firearm carriage is constitutionally protected and difficult to condition, increasing the baseline uncertainty and risk of any stop or call. Cannot decline the encounters that come with the job; their occupational exposure to the effects of the reading is not something they can trade away.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, law_enforcement_officers_facing_armed_encounters, payer,
    moderate, immediate, constrained, local).

% Convicted felons and others subject to categorical firearm prohibitions litigate at the margins of the individual-right reading, arguing the right should extend to them absent individualized dangerousness findings; courts applying the reading have reached inconsistent results, leaving this population in prolonged legal limbo with no reliable restoration pathway. Their voice is present in litigation but structurally marginal to the doctrine's core beneficiary coalition.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, disarmament_ineligible_populations_seeking_restoration, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, disarmament_ineligible_populations_seeking_restoration, excluded).

% Historically regulated firearms as a matter of police power (licensing, sensitive-place restrictions, waiting periods); the individual-right reading, especially combined with the text-history-tradition test, has invalidated or chilled significant categories of this regulation. Legislatures can still act but operate under a shrunken and uncertain constitutional envelope, with many enacted regulations struck down after the fact.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_and_local_legislatures, excluded,
    institutional, generational, constrained, regional).

% Analyze the doctrinal history, textual argument, and empirical consequences of the individual-right reading without a direct material stake in its outcome, though many hold declared normative priors.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable baseline entitlement to keep and bear arms for self-defense that individuals can rely on without needing organizational (militia) status, coordinating expectations across courts, legislatures, and citizens about where the floor of permissible regulation sits.
% TRANSFER_FUNCTION: Moves regulatory latitude away from legislatures and toward individual claimants and the litigation infrastructure that enforces the reading; correspondingly moves risk exposure toward populations most affected by firearm prevalence (domestic violence survivors, high-mortality communities, law enforcement) who bear costs the reading makes harder to legislate against.
% ABSENT_VOICES: Domestic violence survivors and gun-violence-affected communities are rarely direct parties to the individual-rights test cases that shape the doctrine (which are typically brought by license applicants, prohibited possessors, or advocacy-organization plaintiffs); their interests enter mainly through amicus briefs and post-hoc empirical studies rather than as litigants whose exit or voice shapes the rule directly.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight and a collective-security or civic-virtue reading took its place, firearms regulation would revert substantially to ordinary police-power analysis: licensing schemes, carry restrictions, and categorical prohibitions currently vulnerable to constitutional challenge would face far less exposure, the litigation industry built around individual-rights test cases would lose its doctrinal foothold, and legislatures would regain latitude they currently operate without.
% FOUNDING_PROBLEM: The kernel text was framed against a founding-era concern that a standing federal army could disarm state militias and individual citizens, undermining the capacity for organized armed resistance to tyranny and for common defense; the individual-right reading extracts from this a personal self-defense guarantee understood as independent of any militia or civic-defense context.
% FOUNDING_PROBLEM_CORROBORATION: Gun-rights organizations and a majority-opinion line of recent Supreme Court jurisprudence attest that personal self-defense was always the core of the right, independent of militia framing. Historians of the founding era and dissenting justices, positioned outside the beneficiary coalition, contest this characterization, arguing the operative and prefatory clauses were understood by the ratifying public as linked to organized civic defense — the corroboration is genuinely split rather than unanimous, and no source entirely outside the interpretive contest can adjudicate original public meaning with certainty.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).
:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than extreme: the reading does confer a genuine, widely-shared individual entitlement (a real coordination function — citizens can rely on a stable baseline without needing to prove organizational status), but it also transfers regulatory latitude away from legislatures and concentrates litigation-driven doctrinal control in advocacy organizations and industry actors who benefit from an expansive reading. Suppression is moderate (0.35) and has risen over time as the doctrine hardened into an enforceable judicial test that increasingly forecloses legislative alternatives (the text-history-tradition methodology raises the practical bar for any regulation to survive). Theater ratio is modest but rising (0.22) — some of the 'historical tradition' argumentation in later case law has drifted toward selective analogical reasoning rather than settled original-meaning inquiry, a mild Goodhart-style substitution of proxy (surface-level historical analogues) for the underlying interpretive question. Resistance is high (0.72): the reading is fiercely contested by legislatures, public-health researchers, gun-violence-prevention advocates, and a substantial share of constitutional scholarship, and continues to be actively litigated rather than settled.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners, the firearms industry, and gun-rights advocacy organizations sit near the beneficiary end of directionality: the reading directly expands their protected activity, market, and doctrinal leverage, and none faces meaningful exit costs from the constraint (indeed they are mobile/arbitrage with respect to it). Domestic violence survivors, high-mortality communities, and law enforcement sit near the target end: they are structurally trapped or constrained, bear costs the reading makes harder to legislate against, and have no meaningful exit from the ambient risk. The disarmament-ineligible population is a harder case — they are simultaneously excluded from the beneficiary coalition's litigation strategy in practice and yet invoke the same doctrine defensively, producing genuinely mixed directionality captured via the secondary excluded role rather than a directionality override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fear of federal disarmament of citizen militias) is contested as either still-live (in the civic-virtue framing, resistance to tyranny remains a a live concern invoked by the gun-rights coalition) or effectively supplanted (in the individual-right reading itself, which explicitly detaches the guarantee from any militia-context predicate, of a nature critics say abandons rather than fulfills the founding problem). This is precisely the kind of founding-problem status classified as contested rather than adjudicated by this story: the individual-right reading does not claim continuity with organized militia defense at all, making the founding-problem framing partly moot from within its own logic while its critics argue this constitutes drift away from the original coordination function toward an individually-captured entitlement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_function,
    'Does the prefatory militia clause narrow the operative clause''s scope, or is it merely explanatory of one purpose among others the operative clause serves?',
    'Historical linguistic and legal analysis of comparable eighteenth-century constitutional and statutory drafting conventions; examination of contemporaneous state constitutional analogues and ratification-era debates.',
    'If the prefatory clause is held to be limiting, the individual-right reading as authored here loses its textual foundation and collapses toward the collective_security_reading; if explanatory, the individual-right reading''s core premise holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_function, conceptual, 'Whether the militia clause limits or merely explains the operative right.').

omega_variable(
    individual_right_vs_capture,
    'Is the individual-right reading a genuine recovery of an original personal-liberty guarantee, or a doctrine substantially shaped and sustained by an organized advocacy and industry coalition that benefits materially from its persistence?',
    'Trace the funding, litigation strategy, and personnel networks connecting gun-rights advocacy organizations, firearms manufacturers, and the sequence of test cases that established and extended the doctrine; compare against the historical timeline of scholarly and judicial consensus shifts.',
    'If capture-driven, the reading''s coordination story (stable individual liberty baseline) is substantially cover for concentrated beneficiary extraction, supporting a tangled_rope or even snare-leaning computed classification at the beneficiary/payer seats; if genuinely recovered original meaning, the coordination function is closer to load-bearing and the extraction is a byproduct rather than the point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_right_vs_capture, empirical, 'Whether doctrinal entrenchment reflects genuine constitutional recovery or organized beneficiary capture.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly does this reading''s disagreement with the originalist_civic_virtue_reading actually live — is it a disagreement about who held the founding-era right (universal citizenry vs. any individual regardless of civic role), or about whether civic participation is a precondition at all?',
    'Close comparison of the two readings'' treatment of founding-era militia-eligibility statutes (which generally excluded women, enslaved people, and others) against each reading''s claimed universality.',
    'If the disagreement is merely about who counts as part of the militia-eligible citizenry (a scope question), the two readings converge more than this story''s clean separation suggests; if the disagreement is about whether civic-role conditioning applies at all, they are genuinely distinct constraints as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the individual-right and civic-virtue readings are more convergent than this story treats them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__individual_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__individual_right_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1970, second_amendment_text__individual_right_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1970, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_text__individual_right_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement_basis(seco_tr_t2016, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_text__individual_right_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__individual_right_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__individual_right_reading, base_extractiveness, 1791, 0.18).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__individual_right_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1970, second_amendment_text__individual_right_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement_basis(seco_be_t1970, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.34).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2016, second_amendment_text__individual_right_reading, base_extractiveness, 2016, 0.38).
narrative_ontology:measurement_basis(seco_be_t2016, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_text__individual_right_reading, base_extractiveness, 2022, 0.42).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__individual_right_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(seco_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__individual_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__individual_right_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement_basis(seco_su_t1900, observed).
narrative_ontology:measurement(seco_su_t1970, second_amendment_text__individual_right_reading, suppression_requirement, 1970, 0.16).
narrative_ontology:measurement_basis(seco_su_t1970, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.26).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2016, second_amendment_text__individual_right_reading, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement_basis(seco_su_t2016, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_text__individual_right_reading, suppression_requirement, 2022, 0.35).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_text__individual_right_reading, suppression_requirement, 2024, 0.35).
narrative_ontology:measurement_basis(seco_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This story, collective_security_reading, and originalist_civic_virtue_reading are three sibling readings of the same second_amendment_text kernel. They are linked via network edges rather than merged because their ε values, beneficiary/victim structures, and computed classifications differ substantially: this reading computes with concentrated beneficiaries (gun owners, industry, advocacy organizations) and an identifiable victim set, while the collective_security_reading is expected to show a more state-centered coordination function with different extraction dynamics, and the originalist_civic_virtue_reading occupies a distinct civic-participatory frame. Per the ε-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
