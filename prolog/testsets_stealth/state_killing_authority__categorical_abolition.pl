% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: State Killing Authority — Categorical Abolition Reading (Capital Punishment as Practiced)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   Capital punishment as practiced in retentionist jurisdictions: the state
 *   charges a subset of homicides as capital, condemns a subset of those
 *   defendants after trial, and — after years of review — kills them by
 *   procedure. This story is the categorical_abolition reading of the
 *   state_killing_authority kernel (see kernel_context and the committer
 *   omegas): it authors the standing killing arrangement as that reading sees
 *   it — the state taking the lives of persons whose right to life the
 *   reading holds inalienable, under justifications (deterrence, closure,
 *   desert) the reading holds either empirically unsupported or categorically
 *   inadmissible. Per the ε-invariance decomposition rule, the kernel's three
 *   readings are three structurally distinct constraints, not one constraint
 *   viewed from angles: this reading keeps the condemned in the rights-holder
 *   set and places the state in the potential-violator set whenever it
 *   executes; the retributive sibling removes the condemned via forfeiture;
 *   the deterrence sibling makes their status ride an empirical claim. The
 *   stories carry different ε values, different victim sets, and different
 *   failure modes, and are linked via network.affects_constraints. The ε
 *   authored here is for the standing arrangement under contest — the killing
 *   arrangement as practiced — never for the abolitionist arrangement this
 *   reading endorses. KEY AGENTS (by structural relationship):
 *   condemned_prisoners: Primary target (powerless/trapped) — bear the
 *   arrangement's ultimate cost; wrongly_convicted_executed: Irreversibility
 *   victims (powerless/trapped); capital_prosecutors: Agenda-setter and
 *   concentrated recipient (institutional/constrained);
 *   tough_on_crime_politicians: Beneficiary (institutional/constrained);
 *   retributivist_victims_families: Beneficiary (moderate/constrained);
 *   abolitionist_victims_families: Excluded voice (moderate/constrained);
 *   retentionist_appellate_courts: Agenda-setter (institutional/constrained);
 *   execution_apparatus_staff: Enforcement functionaries
 *   (moderate/constrained); human_rights_bodies: Analytical observer
 *   (institutional/analytical). The state's seats are deliberately decomposed
 *   into four institutional functionaries rather than one monolithic actor,
 *   because they hold different exits and different relationships to the
 *   killing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.85).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.76).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, snare).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "State Killing Authority — Categorical Abolition Reading (Capital Punishment as Practiced)").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '0e8ade69-3136-42ed-af12-2975e69472f3').
narrative_ontology:cs_kernel_codification('0e8ade69-3136-42ed-af12-2975e69472f3', formalized).
narrative_ontology:cs_authority_grounding('0e8ade69-3136-42ed-af12-2975e69472f3', lineage).
narrative_ontology:cs_interpretation_layer_present('0e8ade69-3136-42ed-af12-2975e69472f3').
narrative_ontology:cs_reading_relation('0e8ade69-3136-42ed-af12-2975e69472f3', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('0e8ade69-3136-42ed-af12-2975e69472f3', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('0e8ade69-3136-42ed-af12-2975e69472f3', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('0e8ade69-3136-42ed-af12-2975e69472f3', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('0e8ade69-3136-42ed-af12-2975e69472f3', foundational, state_killing_consequence_invariant).
narrative_ontology:cs_axiom_status(state_killing_consequence_invariant, holdable).
narrative_ontology:cs_axiom_grounding('0e8ade69-3136-42ed-af12-2975e69472f3', state_killing_consequence_invariant, deontological).
narrative_ontology:cs_reference_frame('0e8ade69-3136-42ed-af12-2975e69472f3', inalienable_life_rights_framework).
narrative_ontology:cs_drift_state('0e8ade69-3136-42ed-af12-2975e69472f3', contemporary_retentionist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0e8ade69-3136-42ed-af12-2975e69472f3', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, capital_prosecutors).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, tough_on_crime_politicians).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, retributivist_victims_families).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_prisoners).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, wrongly_convicted_executed).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, abolitionist_victims_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, execution_apparatus_staff).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, sovereign_life_authority_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, lex_talionis_retributivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which defendants face capital charges and whether to seek death. The death charge functions as bargaining leverage: the large majority of capital-eligible cases end in negotiated sentences, and offices build careers and docket outcomes on that leverage. They also select which victims' family members speak at sentencing and clemency hearings. Declining to seek death is possible but carries primary-challenge and office-political cost.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, capital_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, capital_prosecutors, beneficiary).

% Campaign on and govern with capital punishment as a signature credential; sign death warrants or grant clemency. Their electoral standing is partly banked in the arrangement's continuation. Reversal carries identifiable primary-electorate costs, so their practical exit from supporting the arrangement is narrower than their formal power to repeal suggests.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, tough_on_crime_politicians, beneficiary,
    institutional, biographical, constrained, national).

% Family members of murder victims who want the killer executed; they receive the execution as desert satisfied and are given standing by prosecutors at sentencing and clemency stages. Their preference is real and their loss if the arrangement ends is real; they do not run the machinery.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributivist_victims_families, beneficiary,
    moderate, biographical, constrained, national).

% Live under sentence of death in restrictive custody, typically for years to decades, while appeals run. They bear the arrangement's ultimate cost. Exit does not exist from their position: clemency and exoneration are the only doors, both controlled by others, and the sentence is carried out by force.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_prisoners, payer,
    powerless, immediate, trapped, national).

% Prisoners executed before their innocence was established, documented in posthumous exoneration cases. They bear the arrangement's only irreversible cost. No remedy exists after the fact; their cases are discovered by others, usually years too late.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, wrongly_convicted_executed, payer,
    powerless, immediate, trapped, national).

% Family members of murder victims who oppose execution. They hold the same moral standing as retributivist families but are structurally marginalized: prosecutors select which families speak, and opposition families are rarely chosen. Organized groups exist, but the arrangement's process gives them no seat.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_victims_families, excluded,
    moderate, biographical, constrained, national).

% Review capital convictions and sentences under statutes and precedent they largely wrote; procedural default, limitation periods, and deference standards seal most exits. They can revisit doctrine, but precedent and political context make reversal costly; the machinery's legitimacy rests substantially on their review function.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retentionist_appellate_courts, agenda_setter,
    institutional, generational, constrained, national).

% Corrections personnel, wardens, and executioners who operate death row and carry out killings. Many bear documented moral injury; turnover and refusal are recurring problems. They can resign individually and the apparatus refills. They hold the machinery but not the agenda.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, execution_apparatus_staff, agenda_setter,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, execution_apparatus_staff, payer).

% Treaty bodies, regional courts, and monitoring organizations that document the arrangement from outside, review retentionist states' compliance, and apply pressure through reporting, extradition refusal, and non-recognition of foreign death sentences. They neither run nor bear the arrangement.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, capital_prosecutors).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement gives the state a single uniform legal channel for its response to the gravest crimes — capital charging rules, sentencing procedures, and mandatory review that resolve, case by case, who may be put to death — and provides a focal point at which the community's maximal condemnation of murder is expressed.
% TRANSFER_FUNCTION: Moves life itself from condemned prisoners to the state's penal power; moves plea concessions from capital-eligible defendants to prosecutors (the capital charge as bargaining threat resolves most such cases by negotiation); moves electoral standing to officeholders who campaign on the arrangement's continuation; moves 'voice of the victims' standing to whichever family members the prosecution selects to speak.
% ABSENT_VOICES: Abolitionist victims' family members would testify against execution but are curated out — prosecutors choose which families speak at sentencing and clemency, and opposition families are rarely chosen. The condemned hold the least standing of anyone in the process that ends them; the wrongly executed can never speak at all. Future defendants — anyone who could be wrongly charged — have no seat anywhere in the arrangement's administration.
% DISAPPEARANCE_RATIONALE: Prosecutors would lose the plea-leverage economy built on capital charging (most capital-eligible cases end in negotiated sentences); capital habeas and post-conviction practice would collapse; political campaigning would lose a signature credential; execution apparatuses would dissolve; families awaiting executions would have their expectations rearranged; and the condemned class would be re-sentenced to the arrangement's alternative. A substantial institutional and political economy is organized around the arrangement's continuation.
% FOUNDING_PROBLEM: The arrangement was built to give the sovereign a proportionate ultimate response to murder — to mark the gravest crime with the gravest penalty (lex talionis proportionality) and to deter the worst offenses by the ultimate threat.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by: the National Research Council's 2012 committee review (the deterrence literature inadequate to establish any effect), human rights treaty bodies and regional courts (UN Human Rights Committee, ECtHR jurisprudence treating abolition as the advancing international standard), victims' family organizations opposing execution (MVFR and survey work showing families split), and the natural experiment of abolitionist peer democracies showing no homicide increase after abolition. Retentionist attestors of a live founding problem sit predominantly inside the benefiting parties (prosecutors' offices, elected officials) or under methodological dispute (a minority of econometric deterrence studies against the NRC finding); no attestation that the arrangement's current operation solves its founding problem exists from outside the benefiting parties.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε 0.85: from this reading's seat the arrangement takes the one thing that cannot be returned, from rights-holders, through a machinery with documented fatal errors (posthumous exonerations) and a plea-leverage economy that takes concessions from the capital-eligible under threat of death. Suppression 0.76: persistence is physical — custody, sealed procedural exits (AEDPA-era habeas curtailment, procedural default, deference standards), and secrecy statutes shielding the killing method; the condemned's exit options are nil. Theater 0.62: as throughput fell from the 1999 peak (98 US executions), the arrangement's activity share shifted from killing toward maintaining the arrangement's image — protocol litigation, clemency ritual, deterrence and closure rhetoric over a shrinking real function; the series crosses 0.5 in the 2007–2016 window, the Goodhart-drift signature. accessibility_collapse 0.78: total for the governed, short of natural-law level because the state retains the abolition alternative. resistance 0.62: moratoria, exonerations, falling death sentences, and international isolation meet the arrangement continuously. Coalition note: the powerless victim seats hold real coalition potential — death-row prisoner litigation, the exoneration movement, and cross-alliance with opposition families (e.g., MVFR) are the historical channels through which the powerless seats have moved the arrangement; part of the measured resistance is that coalition activity. claimed_type snare is this reading's independent structural claim — the coordination story (uniform channel, closure, deterrence) is cover on this reading, persistence depends on coercion, and victims are identifiable — while the metrics are authored descriptively; neither was tuned to the other or to a predicted engine verdict. All three series share one eight-point grid (1972–2026); the scalars are the 2026 endpoint. The macro-arc (rise 1977–1999, decline after) contains moratorium/resumption oscillations (Illinois 2000–2011, California 2006–2019); the scalars were measured at the post-arc, declining-throughput endpoint.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical statutes. From the prosecutor's seat the arrangement is a working instrument: charging discretion, docket control, a plea economy that resolves most capital-eligible cases without trial. From the condemned prisoner's seat it is annihilation with sealed exits. From the retributivist family's seat it is justice finally done; from the opposition family's seat it is a second wrong committed in their name, with a prosecutor deciding whose grief counts. From the appellate court's seat it is a legitimacy problem managed through deference doctrine. The reading's own seat is the analytical abolitionist's, which sees the whole structure at once. The engine computes these per-seat classifications from the declared roles, exits, and directionalities; this story's authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries — capital prosecutors (plea leverage, career capital), tough-on-crime politicians (electoral standing), retributivist victims' families (desert satisfied) — sit near the beneficiary end of d; the arrangement subsidizes them. Declared victims — condemned prisoners and the wrongly executed — sit near the full-target end: trapped exit, national scope, and no post-hoc remedy amplify effective extraction for the condemned class as a whole. Abolitionist victims' families are not coordinated but excluded: the arrangement's machinery curates their absence, a structural relationship the beneficiary/victim arrays alone under-describe — hence their excluded seat. Execution staff enforce without setting the agenda and bear moral-injury costs the beneficiary seats do not; they are enforcement functionaries, not beneficiaries. Suppression is authored as a raw structural property (custody, sealed appeals, forced killing) — the engine scales only extractiveness by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   From this reading the founding problem — a proportionate ultimate response to murder — is dead: retribution is categorically inadmissible by the reading's own premise, and the deterrence half is empirically unproven by the best available review (NRC 2012). The arrangement nonetheless persists and rearranges the world (plea economies, litigation practice, political careers, family expectations), so the dead-problem × world_rearranges mismatch is this reading's capture allegation, cross-checked against the theater path (0.62 and rising as the killing function shrinks). The classification prevents two mislabelings. First, it blocks a rope reading of the arrangement: the uniform-procedure channel is, on this reading, the cover story, and the gains concentrate in a named seat (the prosecutors' plea economy) rather than diffusing. Second, it blocks scoring this story's ε near zero on the strength of the reading's own endorsed norm: the referent is the standing killing arrangement, not the categorical prohibition this reading would entrench — a story about the prohibition itself would be a different constraint with a different profile. If the mandate question were resolved the other way (a live founding problem established), the arrangement would re-examine as at least partly functional and the snare claim would weaken toward tangled_rope; that resolution is the kernel contest itself and is routed to the committer omegas, not settled here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_killing_kernel_reading,
    'This story is one reading of the state_killing_authority kernel — the categorical_abolition reading. What would the sibling readings (retributive_desert, deterrence_instrument) change structurally if adopted instead?',
    'Framework-level choice, not data: the readings are held by different parties; resolution would be a commitment-system shift (constitutional entrenchment of one reading), not an empirical finding.',
    'Adopting retributive_desert removes the condemned from the rights-holder set (forfeiture) and makes the state an authorized executioner with a desert mandate; adopting deterrence_instrument makes the condemned''s status conditional on an unproven empirical claim and converts the categorical prohibition into a cost-benefit test. This story''s victim set, ε, and classification hold only under categorical_abolition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_killing_kernel_reading, conceptual, 'Committer structure: one reading of the state_killing_authority kernel; the disagreement is located at the rights-holder boundary (forfeited, conditional, or inalienable).').

omega_variable(
    foreclosure_structure_of_categorical_reading,
    'Do the sibling readings coexist with this one within a single commitment framework, or does the categorical premise logically exclude them?',
    'Conceptual analysis of the premises: ''inalienable'' entails not-forfeitable (excluding retributive_desert) and ''regardless of consequence'' entails no consequentialist test can justify the killing (excluding deterrence_instrument); no empirical finding changes this — the readings coexist only across parties, never within one framework.',
    'If the relations were coexists_with instead of forecloses, a single framework could blend readings (e.g., abolition with a deterrence exception), which would change the constraint''s victim set and dissolve its categorical structure; the engine''s foreclosure computation from grounding_type and drift_state should confirm the authored relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclosure_structure_of_categorical_reading, conceptual, 'The categorical reading''s exclusivity: inalienability and consequence-invariance logically exclude both sibling premises within any single framework.').

omega_variable(
    wrongful_execution_base_rate,
    'How many executed prisoners were in fact innocent, and what is the base rate of the arrangement''s only irreversible cost?',
    'Posthumous review programs, DNA-era exoneration extrapolations, and matched-case error-rate studies; exoneration registries (e.g., Death Penalty Information Center) as a documented floor, not an estimate.',
    'Each documented wrongful execution is the arrangement''s only unrecoverable cost, borne by an unambiguously innocent rights-holder; a high base rate strengthens the irreversibility argument in any framework and raises the effective extraction borne by the condemned class as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_base_rate, empirical, 'The base rate of fatal error beneath the arrangement''s irreversibility.').

omega_variable(
    victims_family_voice_representation,
    'Does the closure rationale represent victims'' families, or does prosecutorial curation of family speakers manufacture apparent unanimity?',
    'Systematic surveys of capital-case families comparing the full family population against the subset prosecutors platform at sentencing and clemency; tracking of which families are selected and which refused.',
    'If platformed families over-represent pro-execution preferences, the arrangement''s coordination story is thinner than claimed and the excluded seat (abolitionist families) is representative rather than marginal; the closure justification''s weight in any seat''s computed classification shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victims_family_voice_representation, empirical, 'Whether the victims''-family justification is representative or curated.').

omega_variable(
    deterrence_empirical_hinge,
    'Does capital punishment deter murder relative to life imprisonment?',
    'NRC-standard systematic review of panel studies with comparable jurisdictions and capacity controls; the 2012 NRC review found the existing literature inadequate to conclude any effect in either direction.',
    'None for this reading — consequence-invariance holds regardless of the answer; for the deterrence_instrument sibling, the entire permission hangs on it. Authored to mark where the sibling''s empirical hinge sits and that this reading''s classification is invariant to it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_hinge, empirical, 'The sibling reading''s empirical hinge; deliberately classification-invariant for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 1972, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1972, state_killing_authority__categorical_abolition, theater_ratio, 1972, 0.3).
narrative_ontology:measurement(stat_tr_t1977, state_killing_authority__categorical_abolition, theater_ratio, 1977, 0.33).
narrative_ontology:measurement(stat_tr_t1985, state_killing_authority__categorical_abolition, theater_ratio, 1985, 0.36).
narrative_ontology:measurement(stat_tr_t1994, state_killing_authority__categorical_abolition, theater_ratio, 1994, 0.39).
narrative_ontology:measurement(stat_tr_t1999, state_killing_authority__categorical_abolition, theater_ratio, 1999, 0.41).
narrative_ontology:measurement(stat_tr_t2007, state_killing_authority__categorical_abolition, theater_ratio, 2007, 0.49).
narrative_ontology:measurement(stat_tr_t2016, state_killing_authority__categorical_abolition, theater_ratio, 2016, 0.56).
narrative_ontology:measurement(stat_tr_t2026, state_killing_authority__categorical_abolition, theater_ratio, 2026, 0.62).

% Extraction over time
narrative_ontology:measurement(stat_be_t1972, state_killing_authority__categorical_abolition, base_extractiveness, 1972, 0.72).
narrative_ontology:measurement(stat_be_t1977, state_killing_authority__categorical_abolition, base_extractiveness, 1977, 0.78).
narrative_ontology:measurement(stat_be_t1985, state_killing_authority__categorical_abolition, base_extractiveness, 1985, 0.83).
narrative_ontology:measurement(stat_be_t1994, state_killing_authority__categorical_abolition, base_extractiveness, 1994, 0.9).
narrative_ontology:measurement(stat_be_t1999, state_killing_authority__categorical_abolition, base_extractiveness, 1999, 0.93).
narrative_ontology:measurement(stat_be_t2007, state_killing_authority__categorical_abolition, base_extractiveness, 2007, 0.88).
narrative_ontology:measurement(stat_be_t2016, state_killing_authority__categorical_abolition, base_extractiveness, 2016, 0.86).
narrative_ontology:measurement(stat_be_t2026, state_killing_authority__categorical_abolition, base_extractiveness, 2026, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1972, state_killing_authority__categorical_abolition, suppression_requirement, 1972, 0.55).
narrative_ontology:measurement(stat_su_t1977, state_killing_authority__categorical_abolition, suppression_requirement, 1977, 0.62).
narrative_ontology:measurement(stat_su_t1985, state_killing_authority__categorical_abolition, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(stat_su_t1994, state_killing_authority__categorical_abolition, suppression_requirement, 1994, 0.77).
narrative_ontology:measurement(stat_su_t1999, state_killing_authority__categorical_abolition, suppression_requirement, 1999, 0.82).
narrative_ontology:measurement(stat_su_t2007, state_killing_authority__categorical_abolition, suppression_requirement, 2007, 0.8).
narrative_ontology:measurement(stat_su_t2016, state_killing_authority__categorical_abolition, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement(stat_su_t2026, state_killing_authority__categorical_abolition, suppression_requirement, 2026, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% Kernel family decomposition (ε-invariance): 'state killing authority' is one contested kernel; its three readings instantiate three structurally distinct constraints, not one constraint under different observables. This story (categorical_abolition) authors the standing killing arrangement as seen from a rights-holder set that condemnation does not shrink; the retributive sibling authors a desert-mandated killing arrangement with the condemned outside the rights-holder set; the deterrence sibling authors a conditional permission whose ε rides an empirical claim. Different ε, different victim sets, different failure modes — one story per reading, linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
