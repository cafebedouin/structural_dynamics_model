% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Second Amendment Individual Right Reading — Arms Ownership Unconnected to Militia Service
 *   domain: constitutional law / political theory / rights jurisprudence
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   second_amendment_scope: the individual right reading, on which the
 *   operative clause confers on every individual a right to keep and bear
 *   arms unconnected to militia service (Heller 2008; McDonald 2010; Bruen
 *   2022). The standing arrangement under contest — and the epsilon referent
 *   — is the constitutional regime that reading maintains: a judicially
 *   policed national floor under private firearm ownership that removes most
 *   regulatory authority from every legislature. The claimed type is the
 *   reading's own self-presentation: Heller and the advocacy coalition
 *   describe the right as pre-existing and natural, a limit that would hold
 *   whether or not anyone enforced it. That natural-law presentation is
 *   submitted for false-summit evaluation; the authored metrics describe the
 *   arrangement's actual operation — actively enforced by a litigation
 *   coalition, with concentrated gains (ownership security, market shield)
 *   and diffuse costs (narrowed police power, externalized injury). Sibling
 *   readings (collective_right_reading, civic_right_reading) are separate
 *   constraint files, not part of this one; what each would change
 *   structurally is recorded in the omega variables. KEY AGENTS (by
 *   structural relationship): - federal_judiciary:
 *   agenda-setter/administrator (institutional/constrained) — defines the
 *   right's content and polices the regulatory boundary -
 *   individual_firearm_owners: primary beneficiary
 *   (organized/identity-locked) — holds the protected liberty -
 *   firearms_industry: concentrated beneficiary (institutional/arbitrage) —
 *   captures the market shield the guarantee maintains -
 *   gun_rights_advocacy_organizations: enforcement engine and beneficiary
 *   (organized/identity-locked) — litigates the boundary outward -
 *   state_legislatures: payer (institutional/trapped) — bears narrowed
 *   regulatory capacity - municipal_governments: payer (moderate/trapped) —
 *   took the first enforcement wave - gun_violence_victims_communities: payer
 *   (powerless/trapped) — bears the injury side with no adjudicative channel
 *   - public_health_research_community: excluded (organized/constrained) —
 *   its evidence is structurally inadmissible - legal_academy: analytical
 *   observer — sees the full structure from no seat inside it
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter/administrator (institutional/constrained) — sets doctrine, polices the boundary, collects institutional power from the right's scope
 *   - individual_firearm_owners: primary beneficiary (organized/identity-locked) — holds and politically defends the protected liberty
 *   - firearms_industry: concentrated beneficiary (institutional/arbitrage) — captures the market shield; funds the maintenance apparatus
 *   - gun_rights_advocacy_organizations: agenda-setter and beneficiary (organized/identity-locked) — controls the litigation docket that shapes doctrine
 *   - state_legislatures: payer (institutional/trapped) — bears the narrowed regulatory boundary nationally
 *   - municipal_governments: payer (moderate/trapped) — bore the early strikes, judgments, and fees
 *   - gun_violence_victims_communities: payer (powerless/trapped) — bears injury costs with no channel in the governing test; coalition formation observed but insufficient against entrenchment
 *   - public_health_research_community: excluded (organized/constrained) — produces evidence the framework cannot admit
 *   - legal_academy: analytical observer (analytical/analytical) — full-structure view from outside the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.62).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.58).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right Reading — Arms Ownership Unconnected to Militia Service").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional law / political theory / rights jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).
domain_priors:emerges_naturally(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '80cb1d8c-cdab-4464-b2db-cb5564c69a03').
narrative_ontology:cs_kernel_codification('80cb1d8c-cdab-4464-b2db-cb5564c69a03', fixed_text).
narrative_ontology:cs_authority_grounding('80cb1d8c-cdab-4464-b2db-cb5564c69a03', lineage).
narrative_ontology:cs_interpretation_layer_present('80cb1d8c-cdab-4464-b2db-cb5564c69a03').
narrative_ontology:cs_reading_relation('80cb1d8c-cdab-4464-b2db-cb5564c69a03', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('80cb1d8c-cdab-4464-b2db-cb5564c69a03', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('80cb1d8c-cdab-4464-b2db-cb5564c69a03', foundational, individual_right_unconditioned_by_militia_service).
narrative_ontology:cs_axiom_status(individual_right_unconditioned_by_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('80cb1d8c-cdab-4464-b2db-cb5564c69a03', individual_right_unconditioned_by_militia_service, conventional).
narrative_ontology:cs_axiom('80cb1d8c-cdab-4464-b2db-cb5564c69a03', foundational, right_preexists_constitutional_recognition).
narrative_ontology:cs_axiom_status(right_preexists_constitutional_recognition, holdable).
narrative_ontology:cs_axiom_grounding('80cb1d8c-cdab-4464-b2db-cb5564c69a03', right_preexists_constitutional_recognition, deontological).
narrative_ontology:cs_reference_frame('80cb1d8c-cdab-4464-b2db-cb5564c69a03', founding_era_individual_liberty_right).
narrative_ontology:cs_drift_state('80cb1d8c-cdab-4464-b2db-cb5564c69a03', post_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('80cb1d8c-cdab-4464-b2db-cb5564c69a03', '2026-08-10T14:32:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_firearm_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, prospective_firearm_owners).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_legislatures).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, municipal_governments).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_victims_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, originalist_interpretive_method).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, natural_rights_preexistence).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, fourteenth_amendment_incorporation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Second Amendment's text and founding-era record to decide which firearms regulations stand. Since 2008 it has read the amendment as protecting individuals regardless of militia service, and since 2022 it has required regulations to match historical analogues rather than pass means-ends balancing. Its docket, doctrine, and institutional weight grow with the right's scope; it bears none of the arrangement's cost side; and it cannot leave the role without overturning its own recent precedents, which its current composition will not do.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, federal_judiciary, beneficiary).

% Hold hundreds of millions of firearms for self-defense, hunting, and sport under a guarantee that limits what any legislature may restrict. For a large share of owners the firearms are bound up with personal, regional, and political identity, and defending the guarantee is part of maintaining that identity. Exit would mean surrendering property, practice, and community standing; the guarantee itself cannot be exited, only exercised.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_firearm_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Manufactures and sells into a market whose size the guarantee underwrites: because legislatures cannot broadly restrict ownership, demand stays high and product-liability exposure is channeled elsewhere by statute. It funds much of the litigation, scholarship, and political activity that maintains and expands the guarantee, and it captures the resulting revenue while bearing none of the injury costs produced on the arrangement's other side.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Litigate, lobby, and fundraise to defend and extend the reading; their strategic case selection — a leading carry case began as their filing — shapes which questions reach the Court. Membership rolls, donations, and organizational purpose depend on the guarantee staying contested; several of these organizations have fused with the cause to the point where the cause is the organization.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations, beneficiary).

% The unexercised right-holding public: everyone the reading vests with the entitlement whether or not they currently own — the reading's structural delta over the conditioned readings is precisely this universal vesting. Most bear no cost and exercise no right; the guarantee sits dormant until exercised, and individuals step into or out of the owner class freely.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, prospective_firearm_owners, beneficiary,
    moderate, biographical, mobile, national).

% Enact firearms regulation inside a judicially policed boundary that has narrowed sharply since 2022: permit requirements, carry restrictions, and bans on classes of firearms now routinely fail the historical-analogue test. They absorb the loss of regulatory capacity, the fiscal cost of defending struck laws, and the political cost of visible inaction after shootings; their exits are losing appeals or an amendment process that has never approached viability.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_legislatures, payer,
    institutional, generational, trapped, national).

% Concentrated the early restrictive ordinances and so took the first enforcement wave: their handgun and registration schemes were struck beginning in 2008 and their treasuries paid damages and fees. They retain authority over time, place, and manner but not over the core right, cannot opt out of the national floor, and now regulate mainly at the margins the Court has left open.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, municipal_governments, payer,
    moderate, generational, trapped, local).

% Bear the injury and mortality side of widespread civilian armament — daily homicides, a majority of firearm deaths being suicides, accidents, and mass shootings — under a legal framework that gives their interests no adjudicative channel: harm statistics carry no weight in a test that asks only about founding-era analogues. Individually they hold no leverage; their organized successors — survivor networks and national gun-safety nonprofits — litigate at the margins and lose more often than they win.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_victims_communities, payer,
    powerless, biographical, trapped, national).

% Studies firearm injury as a disease burden and proposes regulatory responses grounded in that data. Federal funding for the work was effectively gagged for two decades, and the governing legal test admits none of its evidence, so the community publishes, testifies, and files briefs into a framework structurally closed to what it produces. Its members can leave the field; the field cannot get into the framework.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_health_research_community, excluded,
    organized, generational, constrained, national).

% Produces the constitutional theory, founding-era historiography, and critique on which both the guarantee's defense and its opposition run. It watches the whole structure — text, doctrine, enforcement coalition, cost incidence — from no seat inside it, and its own internecine methodological conflict is part of what the arrangement's enforcement consumes.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, legal_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles by constitutional entrenchment a question ordinary politics could not hold stable: who may possess firearms, and what any legislature may do about it. It gives owners a uniform national guarantee that their arms cannot be broadly confiscated or restricted, gives manufacturers a stable addressable market, and removes firearms governance from recurring majoritarian cycling — at the price of removing it from ordinary democratic revision as well.
% TRANSFER_FUNCTION: Moves regulatory authority over a widely held class of property out of the hands of every legislature and into a judicially policed individual entitlement; moves the injury and mortality costs of widespread civilian armament onto the diffuse public; and moves interpretive power over the boundary to the federal courts, with the litigation coalition controlling which cases define it.
% ABSENT_VOICES: Gun-violence victims and the public-health research community would object and are structurally outside: the historical-analogue test admits founding-era analogues and no contemporary harm data, so the people bearing the arrangement's injury side have no channel in which their objection could register. Also absent from the framework this reading built is the civic-militia constituency whose reading of the prefatory clause the individual right reading erased. They sit in legislatures that keep losing, in public-health journals the courts do not cite, and in dissents that do not command the Court.
% DISAPPEARANCE_RATIONALE: If the individual-right reading vanished overnight — the Court overruling its own precedents and reverting to a collective or militia-conditioned reading — tens of millions of lawfully held firearms would move from protected status to legislative discretion; states would regulate divergently within months; the industry's market assumptions and the advocacy coalition's purpose would collapse; and a decade of doctrine, hundreds of lower-court decisions, and a large historiography apparatus would lose their object. The arrangement's dependence structure is exactly what the stakeholder surface records.
% FOUNDING_PROBLEM: The arrangement, as this reading tells it, secures an individual liberty of armed self-defense that its adherents hold to predate the Constitution; the founding-era text was written amid fears of standing armies and federal disarmament of the people's arms, and the reading carries that assurance forward as a personal right. The civic and militia-federalism problems the prefatory clause names are, on this reading, background purpose rather than operative condition.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties on the core self-defense problem: founding-era state constitutional provisions protecting arms for the defence of self (Pennsylvania 1790, Kentucky 1799) are attested by legal historians across methodological camps, including scholars who reject this reading's scope; criminological literature attests that defensive firearm use occurs while disputing its frequency; and victimization surveys independently document the self-defense need the arrangement serves. No corroboration exists from outside the benefiting parties for the anti-tyranny rationale; on that element the founding problem is attested only by the coalition itself.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_scope__individual_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_scope__individual_right_reading),
    narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 (moderate-high): the arrangement moves regulatory authority over a widely held class of property out of every legislature and into a judicially policed individual entitlement, and its cost side — injury incidence under constrained regulation — lands on parties with no channel to contest the terms, while the benefit side concentrates on owners and the industry. It is not higher because the liberty protected is genuinely exercised and valued by a large minority of the polity and the arrangement does perform a settlement function ordinary politics could not hold stable. Suppression 0.58: the boundary is maintained by judicial review, an organized litigation coalition that challenges every measure, and constitutional entrenchment that forecloses the ordinary legislative exit; it is not higher because no physical coercion is involved and some regulation survives (the domestic-violence-order carve-out). Theater 0.35 and rising in the series: adjudication does real work, but since 2022 a growing share of activity is founding-era historiography performed for courts — the law-office-history critique — rather than functional doctrine. Accessibility_collapse 0.55: alternatives have not fully collapsed — sensitive-place rules, time/place/manner authority, and the recent narrowing persist — but the core regulatory space is closed. Resistance 0.60: every major application is contested, dissents are strong, and several circuits have upheld measures the coalition is appealing. The claim/metrics gap is deliberate and is the datum: the mountain claim is the reading's own natural-right presentation, submitted for false-summit evaluation, while the metrics describe enforced, contested, beneficiary-bearing operation. All three tracked series share one time grid (t = 0, 2, 5, 8, 11, 14, 17), anchored at Heller (0), McDonald (2), and Bruen (14); all points are observed.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's and the advocacy coalition's seats the arrangement presents as a liberty shield: a pre-existing right the courts merely recognize, with resistance read as evidence of the right's necessity. From the state-legislature and victim seats the same structure operates as a transfer: regulatory capacity moves to a protected private interest, injury costs move to the diffuse public, and the historical test closes every channel through which the payers could contest the terms. Same-level lateral dynamics: state_legislatures and municipal_governments hold formally similar seats (both institutional-scale payers, both trapped), but municipalities took the early enforcement wave — their ordinances were struck first and their treasuries paid — while states now litigate scope with deeper capacity; the same boundary lands differently by resources. The judiciary and the advocacy organizations are agenda-setters at different levels: the Court fixes doctrine, the coalition fixes the docket, and neither bears the arrangement's cost side. The powerless payer seat has produced coalition formation — survivor networks, national nonprofits — but coalition power has not breached constitutional entrenchment; it wins statutory margins, not the boundary itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place individual_firearm_owners, firearms_industry, and prospective_firearm_owners near the beneficiary pole; the industry's arbitrage-grade exit (it captures gains and bears none of the cost side) puts it nearest the pole, which is why it is named as the seat the gains accrue to. Victim declarations place state_legislatures, municipal_governments, and gun_violence_victims_communities near the target pole; trapped exit on all three holds them there — no payer can arbitrage out of the national floor. The judiciary sits near the beneficiary pole as administrator: it collects institutional power from the arrangement's scope and bears none of its costs; it is deliberately not listed in the beneficiary arrays because its gain is positional rather than material, and the secondary beneficiary role carries that. Directionality overrides were considered and rejected: the override key is the power atom, and this story's institutional-atom seats (judiciary-beneficiary, industry-beneficiary, legislatures-payer) diverge sharply — one override value would flatten a divergence that is the point of the story, so the structural declarations carry the differentiation instead. Suppression is authored as a raw structural property and is deliberately left unscaled; only extractiveness rides directionality and scope in the engine's arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is preventing two opposite mislabels. First, the reading's own presentation — a pre-existing natural right needing no enforcement because it reflects the moral order — would, taken at face value, certify a mountain and immunize the arrangement from cost accounting. The false-summit evaluation tests that presentation against the structural data: a genuine natural law does not require a standing litigation coalition, a funded historiography apparatus, or an intensifying enforcement test (the suppression series rises 0.40 to 0.58 across the interval, with the step at the 2022 methodology change); that enforcement signature indicates a constructed arrangement held in place by a coalition with concentrated gains. Second, the reverse mislabel: reading the extraction signature as pure snare would erase the genuine coordination the arrangement performs — it settles by entrenchment a question ordinary politics could not hold stable, and the liberty it protects is exercised and valued by tens of millions of people. The shape the data support is real coordination plus asymmetric incidence plus active enforcement, computed per seat by the engine. On mandatrophy: the founding problem (armed individual self-defense) is live, so this is not a dead mandate kept upright by inertia; the open question is the incidence of a live mandate, which is why founding_problem_status is authored live and the dead-mandate mismatch flag should not fire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_constructed_constraint,
    'Is the individual right reading a recognition of a pre-existing natural right that would persist without enforcement, or a constructed constitutional arrangement maintained by an identifiable enforcement coalition (federal judiciary, advocacy organizations, industry funding)?',
    'Comparative institutional analysis: firearm ownership and self-defense practice in peer democracies without an individual constitutional arms right (UK, Australia, Canada). If ownership norms persist there without entrenchment or a standing litigation apparatus, the US arrangement''s persistence is explained by its enforcement coalition rather than by natural right.',
    'If constructed, the false-summit evaluation stands and the arrangement is cost-accounted as enforced extraction with concentrated beneficiaries; if the natural-right reading survives, mountain certification is defensible and the enforcement-intensification series requires reinterpretation as mere doctrinal housekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_constructed_constraint, empirical, 'The FSM ambiguity: the reading''s self-presentation as natural law versus the constructed, coalition-maintained structure the enforcement data suggest.').

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading of kernel second_amendment_scope (reading: individual_right_reading). What would each sibling reading change structurally, and where exactly does the disagreement sit?',
    'Structural comparison across the three reading files: the collective reading removes all individuals from the beneficiary set (the arrangement then restrains only federal interference with state militia authority — extraction collapses toward zero and the victim set empties); the civic reading conditions the right on militia participation (the beneficiary set shrinks to civic participants, non-participant ownership loses protection, and extraction drops with the narrowed coverage). The disagreement is located in the semantic force of the prefatory clause and the referent of ''the people'' in the operative clause.',
    'Adopting either sibling rewrites this story''s beneficiary/victim structure and epsilon; the three files form one family and must never be merged or averaged — each carries its own stable epsilon over the same constitutional text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer structure: which kernel, which reading, what each sibling would change, and the specific structural element (prefatory clause force; referent of ''the people'') where the readings diverge.').

omega_variable(
    originalist_historiography_reliability,
    'Is the founding-era historical record that the post-2022 test consumes determinate enough to ground a stable constraint, or indeterminate enough that outcomes track judicial preference?',
    'Professional historiography audit of the test''s outputs: compare courts'' founding-era analogies against the peer-reviewed literature (the post-Bruen historiography debate); the divergence rate between litigation-generated history and professional consensus measures the performance share of adjudication.',
    'If the record is indeterminate, the theater_ratio series understates the performance share and the arrangement drifts toward theatrical maintenance of a preferred outcome; if determinate, the test is functional adjudication and the theater share is genuinely minor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_historiography_reliability, empirical, 'Whether the historical-analogue test adjudicates or performs founding-era fidelity.').

omega_variable(
    externality_attribution,
    'How much of contemporary firearm injury is attributable to this arrangement''s constraint on regulation, as against independent social causes?',
    'Difference-in-differences violence studies across jurisdictions before and after the 2022 carry expansion, plus cross-national comparison at matched ownership rates.',
    'A large attributable share confirms the victim seat''s directionality and the extraction reading; a small share weakens epsilon toward coordination-cost territory and pulls the classification toward ordinary constitutional rights administration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_attribution, empirical, 'Causal share of injury incidence attributable to the constrained-regulation arrangement itself.').

omega_variable(
    historical_test_functional_bite,
    'Does the historical-analogue test functionally impose strict-scrutiny-level bite on regulation (near-fatal in practice), or does it leave meaningful regulatory space?',
    'Track survival and strike-down rates of post-2022 firearms regulations across circuits, including the domestic-violence-order carve-out upheld in 2024 and the sensitive-places doctrine.',
    'Strict-scrutiny-equivalent bite sustains the high-epsilon reading and the broad-coverage amplification; a permissive test lowers effective extraction and moves the arrangement toward the ordinary rights-administration profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_test_functional_bite, empirical, 'Functional severity of the governing test relative to strict scrutiny.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t2, second_amendment_scope__individual_right_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement_basis(seco_tr_t2, observed).
narrative_ontology:measurement(seco_tr_t5, second_amendment_scope__individual_right_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(seco_tr_t5, observed).
narrative_ontology:measurement(seco_tr_t8, second_amendment_scope__individual_right_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(seco_tr_t8, observed).
narrative_ontology:measurement(seco_tr_t11, second_amendment_scope__individual_right_reading, theater_ratio, 11, 0.27).
narrative_ontology:measurement_basis(seco_tr_t11, observed).
narrative_ontology:measurement(seco_tr_t14, second_amendment_scope__individual_right_reading, theater_ratio, 14, 0.33).
narrative_ontology:measurement_basis(seco_tr_t14, observed).
narrative_ontology:measurement(seco_tr_t17, second_amendment_scope__individual_right_reading, theater_ratio, 17, 0.35).
narrative_ontology:measurement_basis(seco_tr_t17, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t2, second_amendment_scope__individual_right_reading, base_extractiveness, 2, 0.46).
narrative_ontology:measurement_basis(seco_be_t2, observed).
narrative_ontology:measurement(seco_be_t5, second_amendment_scope__individual_right_reading, base_extractiveness, 5, 0.49).
narrative_ontology:measurement_basis(seco_be_t5, observed).
narrative_ontology:measurement(seco_be_t8, second_amendment_scope__individual_right_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(seco_be_t8, observed).
narrative_ontology:measurement(seco_be_t11, second_amendment_scope__individual_right_reading, base_extractiveness, 11, 0.55).
narrative_ontology:measurement_basis(seco_be_t11, observed).
narrative_ontology:measurement(seco_be_t14, second_amendment_scope__individual_right_reading, base_extractiveness, 14, 0.61).
narrative_ontology:measurement_basis(seco_be_t14, observed).
narrative_ontology:measurement(seco_be_t17, second_amendment_scope__individual_right_reading, base_extractiveness, 17, 0.62).
narrative_ontology:measurement_basis(seco_be_t17, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t2, second_amendment_scope__individual_right_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement_basis(seco_su_t2, observed).
narrative_ontology:measurement(seco_su_t5, second_amendment_scope__individual_right_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(seco_su_t5, observed).
narrative_ontology:measurement(seco_su_t8, second_amendment_scope__individual_right_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(seco_su_t8, observed).
narrative_ontology:measurement(seco_su_t11, second_amendment_scope__individual_right_reading, suppression_requirement, 11, 0.52).
narrative_ontology:measurement_basis(seco_su_t11, observed).
narrative_ontology:measurement(seco_su_t14, second_amendment_scope__individual_right_reading, suppression_requirement, 14, 0.57).
narrative_ontology:measurement_basis(seco_su_t14, observed).
narrative_ontology:measurement(seco_su_t17, second_amendment_scope__individual_right_reading, suppression_requirement, 17, 0.58).
narrative_ontology:measurement_basis(seco_su_t17, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Second Amendment' covers three structurally distinct claims with different epsilon, beneficiary sets, and victim sets; per the epsilon-invariance principle they are authored as separate files in one family, each linked to the others here. This file is the individual right reading — the broadest beneficiary set (all individuals, exercised or not), the heaviest restraint on regulatory authority, and the highest epsilon of the family. The collective reading file carries near-zero epsilon (a federalism arrangement with no individual beneficiary set); the civic reading file carries an intermediate structure (a conditioned beneficiary set). The individual right reading currently dominates doctrine, and its enforcement machinery — litigation coalition plus historiography apparatus — is the operating environment both sibling readings contest within; each sibling file should link back to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
