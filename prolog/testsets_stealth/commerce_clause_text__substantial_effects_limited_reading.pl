% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause — Substantial Effects Reading with Jurisdictional Nexus and Non-Pretext Limits
 *   domain: constitutional/legal
 *
 * SUMMARY:
 *   The Commerce Clause authorizes Congress to regulate commerce among the
 *   several States. This story instantiates one reading of that contested
 *   kernel — the substantial-effects-limited reading: federal power reaches
 *   intrastate activity that is genuinely economic in character and
 *   substantially affects interstate commerce, but only through a
 *   jurisdictional nexus and only as non-pretextual economic regulation (the
 *   Lopez–Morrison–Raich line). The reading is structurally hybrid: the same
 *   boundary-policing apparatus that grants national economic regulation also
 *   withholds federal power from non-economic local conduct, and continuous
 *   Supreme Court adjudication is what holds both halves in place. Per the
 *   epsilon-invariance principle, epsilon here refers to the standing
 *   arrangement under contest — the substantial-effects-with-limits doctrine
 *   as it actually operates — never to the sibling readings' arrangements;
 *   the expansive federal reading and the originalist narrow reading are
 *   separate constraints, linked in network.affects_constraints. The
 *   claim/metric relationship is deliberate and unreconciled: claimed_type
 *   records the structure I believe true (tangled rope — genuine coordination
 *   plus asymmetric extraction through one enforced boundary), while the
 *   metrics record the doctrine's actual operation, including its post-2005
 *   drift toward dormant limits.
 *
 * KEY AGENTS:
 *   - supreme_court: agenda setter (institutional, analytical exit) — authors and polices the economic/non-economic line and the nexus requirement; collects the authority of line-drawing
 *   - lower_federal_courts: administering agenda-setters (institutional, constrained) — apply the nexus and characterization tests case-by-case under binding precedent
 *   - congress: primary beneficiary with payer secondary (institutional, arbitrage) — collects the substantial-effects grant; pays in struck statutes and mandatory drafting formalities
 *   - federal_regulatory_agencies: primary beneficiary (institutional, constrained) — accrue enforcement reach over intrastate economic activity; the receipt seat for the extraction
 *   - state_governments: dual beneficiary/payer (institutional, constrained) — retain the non-economic police-power zone; pay when economic characterization overrides state schemes
 *   - intrastate_economic_actors: primary payer (powerless, trapped) — homegrown medical-cannabis patients and home-consumption farmers whose local conduct is federalized by aggregation
 *   - local_harm_claimants: payer on the limit side (powerless, constrained) — denied enacted federal civil remedies because their harms' causes are characterized non-economic
 *   - individuals_in_noneconomic_activity: beneficiary (powerless, constrained) — hold the protected zone from federal overlay
 *   - federal_criminal_defendants: excluded (powerless, trapped) — bear the line's sharpest edge without a seat where it is drawn
 *   - legal_academy: analytical observer — maps the doctrine's drift and the limits' dormancy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.6).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.55).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause — Substantial Effects Reading with Jurisdictional Nexus and Non-Pretext Limits").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional/legal").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, 'cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e').
narrative_ontology:cs_kernel_codification('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', fixed_text).
narrative_ontology:cs_authority_grounding('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', lineage).
narrative_ontology:cs_interpretation_layer_present('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e').
narrative_ontology:cs_reading_relation('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', foundational, commerce_power_reaches_economic_intrastate_activity).
narrative_ontology:cs_axiom_status(commerce_power_reaches_economic_intrastate_activity, holdable).
narrative_ontology:cs_axiom_grounding('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', commerce_power_reaches_economic_intrastate_activity, conventional).
narrative_ontology:cs_axiom('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', foundational, economic_character_is_limiting_principle).
narrative_ontology:cs_axiom_status(economic_character_is_limiting_principle, holdable).
narrative_ontology:cs_axiom_grounding('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', economic_character_is_limiting_principle, deontological).
narrative_ontology:cs_axiom('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', secondary, jurisdictional_nexus_required_for_intrastate_reach).
narrative_ontology:cs_axiom_status(jurisdictional_nexus_required_for_intrastate_reach, holdable).
narrative_ontology:cs_axiom_grounding('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', jurisdictional_nexus_required_for_intrastate_reach, conventional).
narrative_ontology:cs_reference_frame('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', bounded_substantial_effects_doctrine).
narrative_ontology:cs_drift_state('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', post_raich_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cec59d0a-b0b8-4a1e-a694-cfd4d6d6633e', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, congress).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, individuals_in_noneconomic_activity).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, intrastate_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, local_harm_claimants).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, congress).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, enumerated_powers_federalism).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, wickard_aggregation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Through a handful of commerce-clause cases per generation, defines where the economic/non-economic line falls, what counts as a sufficient jurisdictional nexus, and when Congress's economic findings are accepted rather than examined as pretext. Collects the authority of being the line's author; cannot exit the role because the Constitution assigns it the last word, and each retirement reshapes the line without any formal rule changing.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% District and circuit courts apply the doctrine case-by-case: testing jurisdictional elements, accepting or rejecting congressional findings, and characterizing defendants' conduct as economic or not. Their applications are where the nexus requirement succeeds or collapses into boilerplate. They cannot depart from Supreme Court formulations, and most of their commerce-clause rulings receive no appellate review.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, lower_federal_courts, agenda_setter,
    institutional, biographical, constrained, national).

% Drafts statutes relying on the substantial-effects grant — drug control, environmental law, civil-rights public accommodations, firearms regulation. Pays when statutes are struck or when drafting must include jurisdictional elements and economic findings to survive review. Its exit is unusually good: it can recharacterize activity as economic, add nexus elements, or route mandates through the spending power and other enumerated powers.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, congress, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, congress, payer).

% Enforcement arms — drug, environmental, firearms, civil-rights agencies — exercise the authority this reading grants over intrastate activity that aggregates into national markets; prosecutions and administrative actions against purely local conduct are sustained when the class of activity is economic. They accrue the doctrine's principal gains: jurisdiction, prosecutorial discretion, and budget-relevant enforcement volume.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).

% Retain, by the reading's design, police power over violence, family law, education, and other non-economic local matters, and litigate to keep that zone. They pay when Congress characterizes intrastate economic activity as a national market — state medical-cannabis programs, state agricultural schemes, and state licensing regimes are overridden by federal overlay regardless of state law. Exit is limited: states cannot withdraw from federal jurisdiction and cannot shield their residents from private federal enforcement.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, state_governments, payer).

% Growers, patients, farmers, and small businesses whose conduct is entirely intrastate but belongs to a class of activity that is economic — the homegrown medical-cannabis patient compliant with state law, the farmer growing grain for home consumption. Federal criminal or civil liability attaches to their local conduct through aggregation into national markets; state authorization is no defense, and the only exit is ceasing the activity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, intrastate_economic_actors, payer,
    powerless, biographical, trapped, national).

% People harmed by local, non-economic conduct — the gender-motivated violence plaintiff, the student affected by neighborhood gun violence — who look to federal civil remedies Congress enacted for them and find the remedy struck because the cause of their harm is characterized as non-economic. They fall outside the federal reach the reading withholds; state remedies may be inadequate, which is why the federal remedy was enacted, but their remaining option is to pursue whatever state law allows.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_harm_claimants, payer,
    powerless, biographical, constrained, national).

% People whose purely local, non-economic conduct sits inside the protected zone the reading maintains — possessing a firearm near a school, family arrangements, local moral and social conduct. The line shields them from federal criminal overlay so long as their conduct stays non-economic in the Court's characterization; the protection is real but depends on a characterization they do not control.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, individuals_in_noneconomic_activity, beneficiary,
    powerless, biographical, constrained, national).

% Most defendants charged under federal regimes reaching intrastate conduct plead out or lack the resources to litigate the commerce element; the line that defines their liability is set in a handful of Supreme Court cases in which people like them rarely appear. They bear the doctrine's sharpest edge — criminal exposure for local conduct — without a seat where the line is drawn.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_criminal_defendants, excluded,
    powerless, immediate, trapped, national).

% Constitutional scholars map the doctrine's drift — documenting how rarely the limits invalidate statutes after 2005, how aggregation swallows the economic line, and what a revival would require. They shape the interpretive climate future justices inherit but decide nothing themselves.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, legal_academy, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__substantial_effects_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory jurisdiction between the national government and the states for conduct whose effects cross the local boundary: national markets get a single regulator reaching intrastate activity that aggregates into them, while violence, family, education, and other non-economic local matters stay with state police power. The jurisdictional-nexus and non-pretext requirements are the machinery that keeps the allocation tied to commerce rather than to a general federal police power.
% TRANSFER_FUNCTION: Moves regulatory authority and its costs — criminal liability, compliance burdens, enforcement discretion — from intrastate economic actors and state governments to the federal enforcement apparatus, while withholding federal reach from non-economic local conduct, leaving that field and its protection costs with states and individuals.
% ABSENT_VOICES: Federal criminal defendants whose liability the line defines but who almost never obtain appellate review; state legislative majorities whose policy judgments on intrastate economic matters are overridden in litigation their states did not choose to join. Both would object that the line is drawn without them; they appear in the doctrine only as case names.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the federal-state boundary would have to be redrawn at once: every federal statute resting on substantial effects — drug control, environmental regulation, firearms law, public accommodations — would be either vulnerable to facial challenge or freed of any limit, depending on which sibling reading filled the vacuum. State-federal cooperative enforcement, sentencing structures, and decades of reliance drafting would reorganize around whichever reading replaced it.
% FOUNDING_PROBLEM: By the 1990s the operative reading let Congress reach plainly non-economic local conduct under the commerce banner — school-zone gun possession, gender-motivated violence — collapsing the distinction between national economic regulation and a general federal police power. The arrangement was rebuilt to preserve a judicially policed zone of state authority while keeping the New Deal settlement — federal reach into intrastate economic activity with substantial national effects — intact.
% FOUNDING_PROBLEM_CORROBORATION: The federal government attests the original problem is solved and the current line workable; state attorneys general and states'-rights litigants attest that federal reach keeps expanding through aggregation and boilerplate nexus elements. Outside both benefiting parties, the legal academy — spanning defenders and critics of federal power — broadly attests that the limiting half has been largely dormant since 2005, supporting a contested rather than live reading of the founding problem's policing function.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.60): the grant half sweeps purely local economic conduct into federal criminal and regulatory regimes through aggregation (the Raich class), while the limit half strips federal remedies from people whose harms are characterized non-economic (the Morrison class) — the boundary charges both sides of the line. Suppression is 0.55 and is authored as a raw structural property, unscaled by power or scope: targets cannot exit (criminal liability follows the activity regardless of state authorization) and states cannot opt out of the federal overlay, though Congress holds arbitrage-grade drafting exits and the non-economic zone persists, so alternatives are not fully collapsed. Theater is 0.52: the jurisdictional-nexus requirement is routinely satisfied by boilerplate elements, congressional economic findings are accepted at near-rational-basis deference, and the limits have invalidated almost no statutes since 2005 — announced more than applied, though Lopez and Morrison were real strikes and occasional limits still surface. Accessibility collapse is 0.45: the state police-power zone and congressional drafting arbitrage keep workable alternatives partly alive. Resistance is 0.55: states litigate the line repeatedly, scholars contest its administrability, and recurring challenges keep the boundary under pressure. All three tracked series share one time grid (1995–2025, seven points) so every metric is authored at every examined time point; the trajectories tell one story — enforcement of the limits decayed after Raich (suppression_requirement 0.75 to 0.55), the announced limits grew more performative (theater 0.25 to 0.52), and extraction accumulated as the grant half operated while the limit half went dormant (0.50 to 0.60).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seat (the Court), the doctrine is its own craft: a workable, precedent-bound line it polices case-by-case, and the payer seats' complaints read as losing parties' dissatisfaction. From the trapped payer seat (intrastate economic actors), the same structure is a characterization lottery that converts conduct compliant with state law into federal crime. From the constrained payer seat (local harm claimants), it is a closed door — a federal remedy that existed in enacted statute and was withdrawn by characterization. From the beneficiary seats (agencies, Congress), it is an enabling grant with occasional drafting friction. States occupy both halves at once: they collect the protected zone and pay the overridden economic field, so their computed position should sit near symmetric. The engine derives these divergences from the declared roles, power atoms, and exit options; nothing in this commentary adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality toward the beneficiary end for federal_regulatory_agencies (they accrue the enforcement reach and are the receipt seat), congress (collects the grant, with drafting costs pulling slightly back toward symmetric), and individuals_in_noneconomic_activity (they collect the protected zone). Victim declarations drive intrastate_economic_actors toward the full-target end — trapped exit, powerless, liability attaching through aggregation they cannot escape — and local_harm_claimants near the full-target end, since their remedy is withheld by the same boundary that shields others. state_governments sit mid-range by construction: the dual declaration (beneficiary of the limit, victim of the grant) places them near symmetric, which is descriptively right — the reading's value to any state depends entirely on which side of the economic line that state's interest falls. The Court's position is structural rather than subsidy-or-tax: it collects the authority of administering the line and pays docket and legitimacy costs, which the derivation reads from its agenda_setter role and analytical exit rather than from any beneficiary or victim listing. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Reading the doctrine as its self-description — a neutral jurisdictional rope allocating regulatory labor — would erase the extraction: real defendants imprisoned for state-legal conduct, real claimants stripped of enacted remedies. Reading it as a snare — federalism rhetoric as cover for national aggrandizement — would erase the genuine coordination (national markets do require a single regulator reaching local conduct that aggregates into them) and the real limits (Lopez and Morrison did strike statutes; the non-economic zone does shield conduct). The tangled-rope claim holds both: one boundary-policing structure that coordinates and extracts simultaneously, held in place by continuous judicial enforcement. On the R5 genealogy: the founding problem (federal police-power creep into non-economic local life) is authored contested, not dead — the Court still occasionally enforces the line and the federal government still contests its scope — so the mismatch consumer should not expect a dead-mandate flag. The honest open question is whether the policing function is live or dormant; that question is carried by the omega variables rather than by the founding-problem narrative, which is never consumed as a claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the commerce_clause_text kernel (reading: substantial_effects_limited_reading). What would a sibling reading change structurally, and where exactly is the disagreement located?',
    'No dataset resolves a conceptual contest; resolution arrives as doctrinal events — appointments, overrulings, statutory drafting shifts — that change which reading is operative. The disagreement is located in the referent of ''commerce among the several States'' and in who polices the economic/non-economic boundary.',
    'If the expansive reading becomes operative, this story''s limit-half beneficiaries (states, non-economic actors) convert to payers and extraction rises sharply. If the originalist narrow reading becomes operative, the beneficiary set inverts — federal agencies lose reach over intrastate economic activity — and the national-market coordination function atrophies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one reading of a contested kernel, with the structural deltas each sibling would produce.').

omega_variable(
    economic_noneconomic_line_administrability,
    'Is the economic/non-economic distinction judicially administrable, or is characterization outcome-driven — with Raich-style aggregation making every market-participating activity count as economic?',
    'Code lower-court commerce-clause rulings post-2005: does the economic characterization predict outcomes independently of the statute''s subject matter, or does it track the government''s need to reach the defendant?',
    'If the line is indeterminate, the limit half of this reading is theater and the operative constraint collapses toward the expansive reading''s epsilon, with the victim set shifting toward states and non-economic actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_line_administrability, empirical, 'Whether the reading''s distinguishing boundary is a workable test or an outcome-driven label.').

omega_variable(
    aggregation_pretext_vacuity,
    'Does the Wickard/Raich aggregation principle make the non-pretext requirement vacuous — can Congress always characterize any intrastate activity as part of a national economic market?',
    'Catalog non-pretext challenges brought after 2005 and their success rate under the deferential review Raich announced; a near-zero success rate indicates vacuity.',
    'If vacuous, the authored theater_ratio is understated and the reading''s distinguishing axiom (economic character as a limiting principle) operates in name only — pushing the story toward the piton-adjacent question of what, besides rhetoric, the boundary still does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_pretext_vacuity, empirical, 'Whether the non-pretext limit has independent force or is swallowed by aggregation.').

omega_variable(
    limits_revival_pressure,
    'Is the post-2005 dormancy of the limits a stable equilibrium, or a phase awaiting a Court-composition shift that revives active boundary-policing?',
    'Track commerce-clause grants and outcomes across successive Court compositions; a revived invalidation rate, or a docket of credible non-pretext challenges taken up, marks revival_pressure.',
    'Revival would reverse the theater and enforcement-decay trajectories, move the drift_state direction from practice_drift toward revival_pressure, and re-price the extraction borne by federal programs rather than by defendants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(limits_revival_pressure, empirical, 'Stability of the enforcement-decay equilibrium versus composition-driven revival.').

omega_variable(
    federalism_zone_value,
    'Does the preserved zone of state authority over non-economic local matters produce governance value that justifies the line-drawing costs imposed on defendants and claimants on both sides of the boundary?',
    'Not resolvable by data alone; depends on the weight assigned to state autonomy, uniform national markets, and individual exposure to dual sovereignty. Comparative federalism outcomes can inform but not settle the weighting.',
    'If the zone''s value is judged low, the reading''s hybrid structure is extraction with decorative coordination and the tangled-rope claim weakens toward snare; if high, part of the measured extraction is the price of the federalism safeguard itself rather than extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_zone_value, preference, 'Value weighting of the federalism safeguard against the boundary''s certainty and exposure costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(commerce_clause_selr_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement_basis(commerce_clause_selr_tr_t1995, observed).
narrative_ontology:measurement(commerce_clause_selr_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(commerce_clause_selr_tr_t2000, observed).
narrative_ontology:measurement(commerce_clause_selr_tr_t2005, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(commerce_clause_selr_tr_t2005, observed).
narrative_ontology:measurement(commerce_clause_selr_tr_t2010, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement_basis(commerce_clause_selr_tr_t2010, observed).
narrative_ontology:measurement(commerce_clause_selr_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement_basis(commerce_clause_selr_tr_t2015, observed).
narrative_ontology:measurement(commerce_clause_selr_tr_t2020, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2020, 0.5).
narrative_ontology:measurement_basis(commerce_clause_selr_tr_t2020, observed).
narrative_ontology:measurement(commerce_clause_selr_tr_t2025, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(commerce_clause_selr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(commerce_clause_selr_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement_basis(commerce_clause_selr_be_t1995, observed).
narrative_ontology:measurement(commerce_clause_selr_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement_basis(commerce_clause_selr_be_t2000, observed).
narrative_ontology:measurement(commerce_clause_selr_be_t2005, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2005, 0.56).
narrative_ontology:measurement_basis(commerce_clause_selr_be_t2005, observed).
narrative_ontology:measurement(commerce_clause_selr_be_t2010, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement_basis(commerce_clause_selr_be_t2010, observed).
narrative_ontology:measurement(commerce_clause_selr_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement_basis(commerce_clause_selr_be_t2015, observed).
narrative_ontology:measurement(commerce_clause_selr_be_t2020, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement_basis(commerce_clause_selr_be_t2020, observed).
narrative_ontology:measurement(commerce_clause_selr_be_t2025, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(commerce_clause_selr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(commerce_clause_selr_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement_basis(commerce_clause_selr_su_t1995, observed).
narrative_ontology:measurement(commerce_clause_selr_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement_basis(commerce_clause_selr_su_t2000, observed).
narrative_ontology:measurement(commerce_clause_selr_su_t2005, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement_basis(commerce_clause_selr_su_t2005, observed).
narrative_ontology:measurement(commerce_clause_selr_su_t2010, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement_basis(commerce_clause_selr_su_t2010, observed).
narrative_ontology:measurement(commerce_clause_selr_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement_basis(commerce_clause_selr_su_t2015, observed).
narrative_ontology:measurement(commerce_clause_selr_su_t2020, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(commerce_clause_selr_su_t2020, observed).
narrative_ontology:measurement(commerce_clause_selr_su_t2025, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(commerce_clause_selr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the single constitutional label 'the Commerce Clause' decomposes into three structurally distinct constraints — one per reading of the fixed text — because the readings assign different scopes to 'commerce' and therefore different epsilon values, beneficiary sets, and victim sets (epsilon-invariance: one reading, one constraint, one epsilon). This story is the hybrid middle reading. The expansive reading is upstream in institutional practice — its grant half is what this reading retains — and the originalist narrow reading is the persistent dissenting frame that supplies the limit half's normative force. Each file in the family links the others via affects_constraints; the sibling files document this decomposition from their own seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
