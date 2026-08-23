% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Speech Clause - Absolutist Reading ('no law' means 'no law')
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The constitutional text 'Congress shall make no law ... abridging the
 *   freedom of speech' sustains three live readings; this story authors the
 *   absolutist one: 'no law' means no law, and protection is categorical
 *   except for narrow historical exclusions (perjury, fraud, solicitation,
 *   incitement as historically confined, obscenity, core defamation). Hugo
 *   Black is the reading's paradigmatic holder. Structurally the reading
 *   instantiates a constraint with a genuine coordination function - it
 *   removes the official discretion to weigh speech against harm, the
 *   discretion through which the Sedition Act of 1798, the WWI Espionage Act
 *   prosecutions, and the loyalty-program era suppressed dissent - and a
 *   genuine extraction side: the harm costs of protected speech (racist
 *   harassment, intimidation, defamatory mass speech) are externalized onto
 *   targeted minorities whose regulatory remedies the same structure
 *   forecloses. The beneficiary is the speaker class and the majority that
 *   shares the speech commons; the victims are the targeted minority groups.
 *   The extractiveness referent is this standing arrangement - categorical
 *   protection as this reading instantiates it - assessed descriptively; the
 *   sibling readings instantiate different constraints with different
 *   protected sets and victim sets and are authored separately. The claimed
 *   type and the metrics are authored independently: the type states what is
 *   structurally true (both coordination and extraction present, actively
 *   enforced), the metrics state what is descriptively true of the
 *   arrangement's operation.
 *
 * KEY AGENTS:
 *   - political_dissidents: primary beneficiary (moderate/constrained) - protected speakers whose liberty is the arrangement's core product
 *   - press_publishers: concentrated beneficiary (powerful/mobile) - institutional immunity as a concrete commercial asset
 *   - majority_speech_community: diffuse beneficiary with secondary cost-bearing (moderate/mobile) - enjoys the commons, bears misinformation costs
 *   - racially_targeted_minority_groups: primary target (organized/trapped) - bears externalized harm costs with remedies foreclosed
 *   - religiously_targeted_minority_groups: primary target (organized/trapped) - same foreclosure position
 *   - federal_judiciary: agenda-setter (institutional/constrained) - enforces the categorical line through precedent
 *   - would_be_speech_regulators: excluded (institutional/constrained) - regulatory proposals struck down, no seat in the interpretive coalition
 *   - critical_race_scholars: analytical observer (moderate/analytical) - documents externalized costs, proposes alternatives, holds no agenda-setting seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.62).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.58).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Speech Clause - Absolutist Reading ('no law' means 'no law')").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '0a0531cc-222c-4fe5-93a0-36387a337e10').
narrative_ontology:cs_kernel_codification('0a0531cc-222c-4fe5-93a0-36387a337e10', fixed_text).
narrative_ontology:cs_authority_grounding('0a0531cc-222c-4fe5-93a0-36387a337e10', lineage).
narrative_ontology:cs_interpretation_layer_present('0a0531cc-222c-4fe5-93a0-36387a337e10').
narrative_ontology:cs_reading_relation('0a0531cc-222c-4fe5-93a0-36387a337e10', first_amendment_speech_protection__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('0a0531cc-222c-4fe5-93a0-36387a337e10', first_amendment_speech_protection__categorical_balancing_reading, forecloses).
narrative_ontology:cs_axiom('0a0531cc-222c-4fe5-93a0-36387a337e10', foundational, no_law_means_no_law).
narrative_ontology:cs_axiom_status(no_law_means_no_law, holdable).
narrative_ontology:cs_axiom_grounding('0a0531cc-222c-4fe5-93a0-36387a337e10', no_law_means_no_law, deontological).
narrative_ontology:cs_axiom('0a0531cc-222c-4fe5-93a0-36387a337e10', foundational, judicial_balancing_forbidden).
narrative_ontology:cs_axiom_status(judicial_balancing_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('0a0531cc-222c-4fe5-93a0-36387a337e10', judicial_balancing_forbidden, instrumental).
narrative_ontology:cs_reference_frame('0a0531cc-222c-4fe5-93a0-36387a337e10', categorical_no_law_prohibition).
narrative_ontology:cs_drift_state('0a0531cc-222c-4fe5-93a0-36387a337e10', contemporary_doctrinal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0a0531cc-222c-4fe5-93a0-36387a337e10', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, press_publishers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_speech_community).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, racially_targeted_minority_groups).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, religiously_targeted_minority_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, majority_speech_community).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, categorical_textualism).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, anti_balancing_deference_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the speech clause and strikes down statutes that regulate protected speech; maintains the categorical line through precedent across the whole interval. Justices hold life tenure and cannot leave the interpretive role; the institution can move the line only by overruling its own precedents. Derives institutional prestige and jurisdictional reach from owning speech doctrine.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Speak against government policy and majority consensus - socialists, pacifists, civil-rights demonstrators, flag burners. They depend on the categorical shield because they cannot predict which speech a case-by-case official process would spare; historically they were the prosecution targets the reading was built to protect. They have no realistic exit from the jurisdiction's speech regime.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, political_dissidents, beneficiary,
    moderate, biographical, constrained, national).

% Publish investigative reporting, criticism, and offensive content under immunity from prior restraint and, since the actual-malice rule, from most defamation exposure. The immunity functions as a concrete commercial asset; they hold the resources to litigate test cases and to relocate or restructure operations if the line moved.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, press_publishers, beneficiary,
    powerful, generational, mobile, national).

% Ordinary participants in the speech commons: they speak without license and are exposed to speech they did not choose. They enjoy the commons the shield maintains and bear diffuse secondary costs of it - misinformation, offensive expression - which they can largely avoid by disengaging from particular channels.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_speech_community, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__absolutist_reading, majority_speech_community, payer).

% Communities targeted by racist speech: demonstrations outside homes and places of worship, slurs and dehumanizing rhetoric in public discourse, discriminatory leafleting and recruitment marches. The categorical rule forecloses the group-libel and hate-speech-ordinance remedies they once had access to (a group-libel conviction was sustained in 1952; the ordinance route was struck down in 1992). They cannot opt out of being targeted; their organizations' regulatory proposals have repeatedly failed in court.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, racially_targeted_minority_groups, payer,
    organized, generational, trapped, national).

% Religious minorities subjected to harassment, intimidation at assembly, and defamatory mass speech. Same structural position as racially targeted communities: remedies foreclosed, targeting unavoidable, and protection available only against state actors rather than the private speakers who do the targeting.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, religiously_targeted_minority_groups, payer,
    organized, generational, trapped, national).

% State legislatures, city councils, and public university systems that enact hate-speech codes, group-libel statutes, and harassment ordinances. Their enactments are struck down under the categorical line; they hold no seat in the interpretive coalition that sets speech doctrine and can only re-legislate in narrower forms that fail again.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, would_be_speech_regulators, excluded,
    institutional, biographical, constrained, national).

% Legal scholars and litigators who document the costs the categorical rule externalizes - dignitary harm, silencing effects, discriminatory environment formation - and propose doctrinal alternatives. They publish, submit amicus briefs, and litigate test cases, but their proposals have not been adopted and they hold no agenda-setting seat.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, critical_race_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of government censorship: by removing official discretion to weigh speech against harm, it eliminates the licensing and balancing mechanism through which majorities historically suppressed dissent (the Sedition Act of 1798, the WWI Espionage Act prosecutions, the loyalty-program era). A bright line is cheaper to administer than a balancing test and gives speakers certainty ex ante.
% TRANSFER_FUNCTION: Moves immunity from regulation to all speakers within the protected set regardless of the harm their speech causes, and correspondingly moves the costs of harmful speech - dignitary harm, harassment, intimidation, hostile environment formation - onto targeted groups who bear them without legal remedy. It also moves the permissibility decision over speech from government officials to speakers themselves.
% ABSENT_VOICES: The targeted minorities who bear the arrangement's costs are formally citizens but structurally voiceless in its construction: the cases are litigated between speakers and the state, and hate-speech victims hold no seat. Would-be regulators are excluded by the rule itself - their policy instrument is what the enforcement machinery strikes down. Critical race scholars articulate the objection from law reviews and dissents (the group-libel lineage, the ordinance campaigns) but hold no agenda-setting seat and their proposals have not been adopted.
% DISAPPEARANCE_RATIONALE: Absolutists predict rearrangement: without the bright line, official balancing returns and dissent suppression resumes as it did after 1798 and during WWI. Harm-limited advocates predict no rearrangement: other democracies maintain hate-speech regulation alongside robust speech cultures, and the private platform layer already governs most contemporary discourse. The parties genuinely dispute which world would follow, so the verdict is authored as contested rather than resolved.
% FOUNDING_PROBLEM: Official suppression of political dissent: the Sedition Act prosecutions of 1798 and the Espionage Act convictions of WWI showed that judicial balancing of speech against harm reliably collapsed into deference to executive and majoritarian suppression. The categorical reading was built to remove the discretion that made such suppression possible.
% FOUNDING_PROBLEM_CORROBORATION: The historical founding problem is corroborated from outside the benefiting parties: the Sedition Act's contemporaneous congressional and state-legislature opposition, the Harding-era pardons of WWI political convictions, and standard press-freedom historiography all document the suppression pattern - none of these are speakers' advocacy organizations. The claim that the problem is now dead is attested by comparative constitutional scholarship documenting robust speech cultures under hate-speech regulation in other democracies, and by critical-race and feminist scholarship documenting the arrangement's ongoing costs. The claim that it remains live is attested by the recurring documented pattern of anti-protest statutes, flag-desecration amendment campaigns, and wartime prosecution proposals; the monitoring organizations that report this pattern themselves benefit from the reading, which is why the recurring-attempt record, not their testimony, carries the corroboration.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, contested).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the arrangement's operative effect divides between the speech commons it produces and the harm costs it externalizes onto targeted minorities; extraction is substantial but non-confiscatory - the arrangement takes no resource from its targets, it forecloses their remedy while conferring immunity on speakers. Suppression 0.58: structural foreclosure of alternatives - a group-libel remedy existed and was used (a 1952 conviction was sustained) and was later closed by doctrine (1992), so the foreclosure is of a real avenue, not a hypothetical one; suppression is authored as the raw structural property it is, unscaled by power or scope. Theater 0.25: the protective function is genuinely performed - courts actually strike down speech regulations across the interval - but invocations grow more ceremonial in the platform era as discourse governance migrates to private moderation. Accessibility_collapse 0.35: alternatives persist and the line demonstrably moves (group libel upheld in 1952, the ordinance route struck down in 1992; private platform governance; other democracies' models), so alternatives do not fully collapse. Resistance 0.55: sustained critical scholarship, repeated ordinance and campus-code campaigns, recurring legislative attempts - organized resistance the arrangement consistently defeats but never eliminates. All three tracked metrics run on one shared nine-point grid (t=0 corresponds to 1940, t=80 to 2020), authored at every point; the trajectories are rise-then-plateau rather than cyclical. suppression_requirement is tracked because the story specifically traces enforcement-capacity change: the doctrinal consolidation era built up the striking-down machinery, which then partially stands down as governmental regulatory attempts decline in the platform era.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the point. From the dissident and press seats the arrangement is a shield: the engine should compute beneficiary-damped effective extraction there, approaching subsidy for the most dependent speakers. From the trapped, organized, generationally-horizoned minority seats the same structure operates as foreclosure: full-target directionality with no exit damping. From the judiciary seat it is doctrine management - the enforcer neither pays the harm costs nor consumes the commons. From the would-be-regulator seat it is a bar on its preferred policy instrument. These divergences are computed from the authored role, power, exit, and scope data, not asserted by the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the beneficiary end of d: political_dissidents (constrained exit, high dependence) and press_publishers (mobile, resource-backed) both receive the arrangement's product - immunity. majority_speech_community sits near symmetric: genuine commons benefit against diffuse secondary costs, carried as a secondary payer role. Victims map to the target end: the racially and religiously targeted groups bear the externalized costs, are trapped with no exit from being targeted, and hold a generational horizon - the derivation should place them near full-target. One override is declared: the institutional atom is set to d=0.25 for the federal_judiciary, because the structural derivation has no beneficiary or victim declaration for the enforcer and would fall back to a canonical institutional default; the true relationship is a modest self-benefit - institutional prestige and jurisdictional reach from owning speech doctrine - that is neither capture of the extraction nor target-hood. Receipt surface: the gains are the speech commons itself, accruing diffusely across the speaker class; no named seat concentrates the extraction's proceeds (press immunity is the most concentrated single benefit but is a share of the commons, not the receipt point of what is taken from the targets), so gain_flow is authored as the affirmative checked claim 'diffuse'. fixing_cost is authored prohibitive on its own evidence: removal requires constitutional amendment or overruling a consolidated doctrinal line, and reopening official discretion reinstates the founding risk the rule was built against - the cost to whoever could fix it exceeds the benefit relative to the diffuse gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - official suppression of dissent through discretionary balancing - is authored contested: recurring suppression attempts (anti-protest statutes, flag-desecration amendment campaigns, wartime prosecution proposals) keep the coordination function live, so the mandate has not clearly outlived the function and mandatrophy_resolved is not declared. The classification guards against two opposite mislabels. Reading the arrangement as pure coordination would erase the externalized victim class the structural data names; reading it as pure extraction would erase the historically documented anti-censorship function, corroborated from outside the beneficiary set by the Sedition Act record and the WWI prosecution history. The theater series' late rise (0.14 at t=40 to 0.25 at t=80) is the leading indicator of a decay path: if discourse governance completes its migration to private moderation while ceremonial invocations of the text keep growing, the arrangement could drift toward theatrical maintenance of a nominally live function. Note on the receipt cell: prohibitive fixing cost combined with diffuse gains is the combination the piton cell describes, but this arrangement is fully operative rather than inertial - low theater, live enforcement, real function - so the cell combination here reflects entrenchment plus diffuse gains, not atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the absolutist_reading of the first_amendment_speech_protection kernel; how would the sibling readings (harm_limited_reading, categorical_balancing_reading) restructure the beneficiary/victim set and the extractiveness profile?',
    'Classify the sibling stories separately and compare protected-set boundaries, victim sets, and epsilon; the disagreement is located in whether harm and official discretion can defeat categorical protection.',
    'Under the harm-limited reading the victim set shifts from targeted minorities toward speakers facing regulation and extraction redistributes toward government discretion; under the categorical-balancing reading the agenda-setter seat gains discretion power and the bright line dissolves into case-by-case adjudication.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story is one of three readings of the same constitutional kernel; sibling readings are separate constraints, not folded into this one.').

omega_variable(
    externalized_harm_magnitude,
    'How large are the measurable costs the categorical rule externalizes onto targeted minorities - health, economic, and civic-participation effects of targeted harassment, racist speech, and intimidating mass speech?',
    'Social-science measurement: longitudinal studies of targeted-harassment outcomes, and comparative studies of jurisdictions that regulate hate speech against matched jurisdictions that do not.',
    'Large measured harms push epsilon upward and tilt the classification toward the extractive pole; modest measured harms support the coordination-dominant reading and lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_harm_magnitude, empirical, 'Magnitude of the externalized harm costs that constitute the extraction side of the arrangement.').

omega_variable(
    balancing_deference_thesis_status,
    'Is the instrumental axiom''s empirical premise - that judicial balancing of speech against harm collapses into deference to suppression - still true of contemporary courts?',
    'Comparative doctrinal analysis: balancing regimes in other democracies'' speech jurisprudence, and post-1960s balancing in other US rights domains, tested for systematic deference to suppression.',
    'If balancing no longer systematically defers, the categorical rule''s justification narrows to its deontological core, the reading''s instrumental defense weakens, and the coordination story would need re-grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_deference_thesis_status, empirical, 'Whether the empirical premise under the anti-balancing axiom still holds.').

omega_variable(
    remedy_foreclosure_classification,
    'Does the arrangement''s foreclosure of legal remedies for targeted speech count as suppression of the victims'' alternatives, or as the mere absence of a benefit the arrangement was never structured to provide?',
    'Conceptual analysis against the framework''s suppression definition, using the historical test: whether victims had a pre-existing alternative avenue that the constraint''s operation closed (a group-libel remedy existed and was used before later doctrine closed it).',
    'If foreclosure counts as suppression, the suppression value is honestly high and the arrangement tilts extractive; if not, suppression is low and the arrangement looks closer to coordination with externalities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_foreclosure_classification, conceptual, 'Whether the foreclosure of minority remedies is suppression of alternatives or non-provision of a benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_absolutist_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(fa_absolutist_tr_t0, observed).
narrative_ontology:measurement(fa_absolutist_tr_t10, first_amendment_speech_protection__absolutist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(fa_absolutist_tr_t10, observed).
narrative_ontology:measurement(fa_absolutist_tr_t20, first_amendment_speech_protection__absolutist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(fa_absolutist_tr_t20, observed).
narrative_ontology:measurement(fa_absolutist_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(fa_absolutist_tr_t30, observed).
narrative_ontology:measurement(fa_absolutist_tr_t40, first_amendment_speech_protection__absolutist_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement_basis(fa_absolutist_tr_t40, observed).
narrative_ontology:measurement(fa_absolutist_tr_t50, first_amendment_speech_protection__absolutist_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement_basis(fa_absolutist_tr_t50, observed).
narrative_ontology:measurement(fa_absolutist_tr_t60, first_amendment_speech_protection__absolutist_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(fa_absolutist_tr_t60, observed).
narrative_ontology:measurement(fa_absolutist_tr_t70, first_amendment_speech_protection__absolutist_reading, theater_ratio, 70, 0.21).
narrative_ontology:measurement_basis(fa_absolutist_tr_t70, observed).
narrative_ontology:measurement(fa_absolutist_tr_t80, first_amendment_speech_protection__absolutist_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement_basis(fa_absolutist_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(fa_absolutist_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(fa_absolutist_be_t0, observed).
narrative_ontology:measurement(fa_absolutist_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(fa_absolutist_be_t10, observed).
narrative_ontology:measurement(fa_absolutist_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(fa_absolutist_be_t20, observed).
narrative_ontology:measurement(fa_absolutist_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(fa_absolutist_be_t30, observed).
narrative_ontology:measurement(fa_absolutist_be_t40, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(fa_absolutist_be_t40, observed).
narrative_ontology:measurement(fa_absolutist_be_t50, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(fa_absolutist_be_t50, observed).
narrative_ontology:measurement(fa_absolutist_be_t60, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement_basis(fa_absolutist_be_t60, observed).
narrative_ontology:measurement(fa_absolutist_be_t70, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 70, 0.64).
narrative_ontology:measurement_basis(fa_absolutist_be_t70, observed).
narrative_ontology:measurement(fa_absolutist_be_t80, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement_basis(fa_absolutist_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(fa_absolutist_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(fa_absolutist_su_t0, observed).
narrative_ontology:measurement(fa_absolutist_su_t10, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(fa_absolutist_su_t10, observed).
narrative_ontology:measurement(fa_absolutist_su_t20, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(fa_absolutist_su_t20, observed).
narrative_ontology:measurement(fa_absolutist_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(fa_absolutist_su_t30, observed).
narrative_ontology:measurement(fa_absolutist_su_t40, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(fa_absolutist_su_t40, observed).
narrative_ontology:measurement(fa_absolutist_su_t50, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement_basis(fa_absolutist_su_t50, observed).
narrative_ontology:measurement(fa_absolutist_su_t60, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(fa_absolutist_su_t60, observed).
narrative_ontology:measurement(fa_absolutist_su_t70, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 70, 0.6).
narrative_ontology:measurement_basis(fa_absolutist_su_t70, observed).
narrative_ontology:measurement(fa_absolutist_su_t80, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(fa_absolutist_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'First Amendment free speech protection' covers three structurally distinct constraints - one per reading of the same kernel text - per the epsilon-invariance decomposition rule. This story authors the absolutist reading only: maximal protected set, categorical enforcement, externalized harm costs borne by targeted minorities. The sibling stories author different protected sets (harm-yielding; balancing-produced) with different victim sets and different epsilon values over the same fixed text. The categorical doctrines consolidated under this reading (the actual-malice rule, incitement doctrine, the ordinance strikes) form the legitimacy terrain on which the sibling readings argue, so this reading sits upstream of both siblings in the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__absolutist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
