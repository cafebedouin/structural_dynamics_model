% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Individual-Right Arms Boundary: Operative-Clause Shield on Private Possession
 *   domain: constitutional law/political theory/firearms policy
 *
 * SUMMARY:
 *   A constitutional boundary, operative since ratification and hardened into
 *   its modern form by the Heller (2008), McDonald (2010), and Bruen (2022)
 *   line, shields private firearm possession as a pre-existing individual
 *   right: the operative clause protects, and state regulation of possession
 *   or carry is presumptively an infringement, survivable only by historical
 *   analogue. The boundary settles the citizen-state arms relationship — no
 *   level of government may disarm the lawful populace — while the same
 *   structure insulates the firearms market from prohibition and forecloses
 *   the regulatory instruments through which exposed populations would
 *   otherwise reduce their exposure. Benefits concentrate on owners,
 *   manufacturers, and advocacy organizations; costs concentrate on those
 *   exposed to unrestricted access: mass-casualty events, domestic homicide
 *   where an abuser has firearm access, and the roughly half of U.S. firearm
 *   deaths that are suicides. The interval maps 2008-2026 (t=0 is Heller;
 *   t=15 is Bruen's first full appellate year).
 *
 * KEY AGENTS:
 *   - private_firearm_owners: primary beneficiary (organized/identity_locked) — possession shielded from prohibition; exit is identity-costly
 *   - firearms_industry: primary beneficiary (institutional/arbitrage) — collects the market-side gains: insulated demand and civil-liability immunity
 *   - gun_rights_advocacy_organizations: beneficiary (institutional/identity_locked) — the boundary is both their cause and their resource base
 *   - federal_judiciary: agenda_setter (institutional/identity_locked) — administers the boundary and its historical-analogue test
 *   - mass_shooting_victims: primary payer (powerless/trapped) — bears the costs of foreclosed prevention
 *   - domestic_violence_victims: primary payer (powerless/trapped, immediate horizon) — abuser firearm access
 *   - firearm_suicide_victims: primary payer (powerless/trapped, immediate horizon)
 *   - urban_communities_with_concentrated_gun_violence: diffuse payer (powerless/trapped)
 *   - state_regulatory_authorities: payer (institutional/constrained) — regulatory capacity foreclosed
 *   - gun_violence_prevention_advocates: excluded (organized/constrained) — expertise inadmissible in the interpretive forum
 *   - public_health_researchers: analytical observer
 *   - constitutional_law_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.6).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Individual-Right Arms Boundary: Operative-Clause Shield on Private Possession").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional law/political theory/firearms policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '20f68ba0-cb27-4b2b-a168-ee7de7ce38d3').
narrative_ontology:cs_kernel_codification('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', fixed_text).
narrative_ontology:cs_authority_grounding('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', lineage).
narrative_ontology:cs_interpretation_layer_present('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3').
narrative_ontology:cs_reading_relation('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', foundational, pre_existing_individual_right_to_arms).
narrative_ontology:cs_axiom_status(pre_existing_individual_right_to_arms, holdable).
narrative_ontology:cs_axiom_grounding('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', pre_existing_individual_right_to_arms, deontological).
narrative_ontology:cs_axiom('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', foundational, prefatory_clause_does_not_limit_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_does_not_limit_scope, holdable).
narrative_ontology:cs_axiom_grounding('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', prefatory_clause_does_not_limit_scope, conventional).
narrative_ontology:cs_reference_frame('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', founding_era_individual_arms_liberty).
narrative_ontology:cs_drift_state('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', post_bruen_doctrinal_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('20f68ba0-cb27-4b2b-a168-ee7de7ce38d3', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, private_firearm_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, firearm_suicide_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, urban_communities_with_concentrated_gun_violence).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_regulatory_authorities).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, pre_existing_rights_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, originalist_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own firearms for self-defense, hunting, sport, and collection; the boundary shields possession from prohibition, strikes registration and storage mandates, and constrains carry licensing. They experience the arrangement as protection of a liberty they hold to pre-date government. Relinquishing arms is legally possible but culturally penalized and bound up with a self-reliance self-concept, so exit from the protected class is identity-costly rather than impractical.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, private_firearm_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Manufactures and sells into a market whose demand is insulated from prohibition by the constitutional boundary, with civil-liability immunity layered on by statute. Demand surges whenever regulation is threatened. It could retool to other product lines, but the domestic market is the profit core, so the boundary functions as a standing demand guarantee and the seat where the arrangement's market-side gains demonstrably accrue.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Litigate, lobby, and mobilize members in defense of the boundary; membership and donation flows track the perceived threat to the right, so each regulatory attempt replenishes the resource base. The organizations' institutional identity has fused with the right's defense — pivoting away would dissolve them — making the arrangement simultaneously their cause and their operating budget.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations, beneficiary,
    institutional, generational, identity_locked, national).

% Administers the boundary: defines the historical-analogue test, strikes or stays regulations, and resolves circuit splits. The current majority's interpretive method is fused with the boundary's maintenance, so revisiting the frame would require the court to spend institutional capital against its own method. It sets the agenda for what counts as a permissible regulation and enforces the result.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Killed, wounded, or bereaved in mass-casualty events. The class of regulations with the strongest evidence of reducing casualty counts — assault-weapon and large-magazine limits — is struck or chilled under the boundary, so the preventive instrument was foreclosed before the harm arrived. Survivors carry injury and grief with no legal lever against the arrangement; their recourse is political voice, which constitutional supremacy overrides.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, biographical, trapped, national).

% Face a partner or ex-partner with firearm access, which multiplies homicide risk several-fold; leaving is the most dangerous moment. Protective-order disarmament survives only within contested doctrinal margins, and the boundary constrains the broader disarmament statutes that would protect them. They can exit a relationship but not the abuser's access to arms, so the exposure follows them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, national).

% Constitute the majority of U.S. firearm deaths; means lethality converts an attempt into a death. Waiting periods and extreme-risk protection orders — the instruments with evidence of interrupting this pathway — are constrained in whole or part by the boundary. The decedents cannot speak; the class is represented by bereaved families and the epidemiological record, and its members exit only by dying.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_suicide_victims, payer,
    powerless, immediate, trapped, national).

% Bear a concentrated homicide burden, disproportionately in Black urban neighborhoods: cumulative trauma, economic drag, and policing burden across generations. The boundary forecloses the jurisdiction-level regulatory tools these communities' governments would otherwise deploy. Moving away is available only to those with resources, so the class as such is held in place.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, urban_communities_with_concentrated_gun_violence, payer,
    powerless, generational, trapped, regional).

% Legislatures and agencies in high-violence states enact licensing regimes, storage mandates, assault-weapon bans, and risk-protection orders; a large share is struck or chilled under the historical-analogue test, at continuing litigation cost. They cannot exit the constitutional framework — only re-craft statutes as historical analogues — so their policy capacity is foreclosed rather than their persons harmed.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_regulatory_authorities, payer,
    institutional, generational, constrained, regional).

% Organize for evidence-based regulation and litigate for it, but their epidemiological expertise is structurally inadmissible in the forum that sets the boundary, which admits founding-era evidence rather than modern data. They argue from outside the interpretive conversation that decides the question, with mobilization capacity that moves politics but not the governing test.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_violence_prevention_advocates, excluded,
    organized, biographical, constrained, national).

% Quantify firearm mortality and the effects of regulatory instruments; their findings define the size and distribution of the harm-side ledger but carry no vote in the interpretive forum. They see the full cost structure without the power to alter it.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_health_researchers, observer,
    moderate, biographical, analytical, national).

% Map the doctrine's structure, history, and failure modes from both originalist and living-constitutionalist seats; they document how the boundary operates and where its method strains, without holding enforcement power. The analytical seat from which the full arrangement is visible.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, constitutional_law_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the citizen-state boundary over arms: a constitutionally entrenched individual right coordinates the expectation that no level of government may disarm the lawful populace, converting what would otherwise be a recurring asymmetric political fight (each citizen alone against the state) into a settled rule individuals can rely on without collective action.
% TRANSFER_FUNCTION: Moves regulatory authority from legislatures to courts, market security (insulated demand, civil-liability immunity) to owners, manufacturers, and advocacy organizations, and the costs of unrestricted access — death, injury, trauma, and foreclosed prevention — to exposed persons and their communities.
% ABSENT_VOICES: Firearm suicide decedents cannot appear; mass-shooting victims appear only as bereaved survivors after the fact; public-health evidence is structurally inadmissible under the historical-analogue test; regulators in high-violence jurisdictions argue from outside the interpretive forum. The apparent settledness of the interpretive conversation is partly an artifact of whose evidence its rules admit.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, states would re-regulate within legislative sessions — licensing, safe-storage mandates, assault-weapon and magazine limits, expanded risk-protection orders — the market would contract under liability exposure, carry regimes would revert to discretionary issuance, and advocacy funding would reorganize around ordinary politics. The post-Bruen litigation wave demonstrates the dependence in reverse: the arrangement holds exactly where the constitutional shield holds.
% FOUNDING_PROBLEM: Founding-era fear of a standing army and federal disarmament of the populace: the arrangement was constitutionalized so that the people could not be disarmed, with the individual-right reading locating the protected problem in the individual's need for arms for self-defense and as a check on centralized force.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era historians outside the beneficiary set attest the anti-standing-army genealogy and dispute how individual or collective the original right was; public-health researchers attest that the arrangement's operative modern problem is not federal disarmament but firearm harm; advocacy organizations attest the disarmament threat remains live. No party outside the beneficiary set attests the founding problem is simply solved, and none attests it is simply unchanged — the corroboration is itself the contest.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.65: substantial asymmetric costs flow through the same structure that protects lawful possession — not total, because the right also shields genuine self-defense, hunting, and sport uses that their holders value. Suppression 0.6 is a raw structural property, unscaled by power or scope: the foreclosure of the regulatory alternative operates through constitutional supremacy, not physical coercion, and it leaves room — some regulations survive as historical analogues. Theater_ratio 0.3: the boundary's operation is mostly functional (courts actually strike and stay laws), but a growing share of activity is performative — the prefatory clause is rhetorically maintained without any limiting function under this reading, and historical-analogue lawyering multiplies citations to founding-era statutes of doubtful relevance. Accessibility_collapse 0.6: once the boundary is understood, comprehensive regulation collapses as a legal option, though the analogue carve-out keeps partial alternatives alive. Resistance 0.65: sustained mass movements, legislative counter-efforts in high-violence states, and a continuous litigation wave. Claim/metric independence: the type is claimed as tangled_rope from the structural read — a genuine coordination function (the citizen-state boundary solves a real collective-action problem) plus asymmetric imposition (victims pay through the same structure) plus active judicial enforcement — while the metrics are authored as descriptive of the operation; any divergence between claim and computed type is data, not error. The three measurement series share one time grid (t=0,3,6,9,12,15,18); suppression_requirement is authored because the story specifically tracks enforcement intensification (the Bruen ratchet: an expanding doctrinal apparatus striking and staying regulation), not merely a shift in extraction. gain_flow names firearms_industry because the market-side gains — insulated demand plus civil-liability immunity — demonstrably accrue there; fixing_cost is prohibitive because the fix paths are Article V amendment or doctrinal reversal by a court whose method is invested in the current frame.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the owner and industry seat the boundary is bedrock: a pre-existing liberty that government may not abridge, with imposed costs near zero and the fixity of natural law claimed for a text. From the victim seats the same structure is enforced imposition with trapped exit — the preventive instruments were foreclosed before the harm arrived, and recourse is political voice overridden by constitutional supremacy. The judiciary seat experiences neutral administration of text; the excluded advocacy seat experiences foreclosure of its entire evidence base from the interpretive forum. The victim seats are individually powerless; their coalition capacity (mass mobilization after high-casualty events) is the visible source of the resistance metric, and it has so far moved legislatures faster than courts. The engine computes these per-seat types from power, exit, and role; the gap between the reading's self-assessment and the victim-seat accounting is precisely the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Owners are declared beneficiaries with identity_locked exit: the derivation places them on the benefit side of the ledger, and the identity lock binds them to the arrangement rather than to its costs — they cannot cheaply leave the protected class either. The industry is a beneficiary with arbitrage-grade exit, placing it nearest the beneficiary end: it captures the market-side gains and can retool if the frame shifts. Advocacy organizations are beneficiaries whose institutional identity is fused with the arrangement. The four harm-bearing groups are payers with trapped exit and powerless or organized power, placing them near the full-target end; state regulatory authorities are payers with constrained exit (they cannot exit the Constitution, only craft analogues), at moderately high directionality. The judiciary is the agenda-setter: it administers the boundary and is not a rent collector, so its position derives from its enforcement role rather than from the beneficiary/victim ledger. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the correct relationships, and the one coarse spot (owners are both organized and identity-locked) is resolved on the benefit side by the role declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite mislabels. Reading the arrangement as pure extraction erases the genuine coordination function: the citizen-state boundary solves a real collective-action problem that tens of millions of owners would rebuild if it fell, and the ownership alternative is not suppressed — the arrangement is not held up by coercion of its beneficiaries. Reading it as pure coordination erases the asymmetric ledger: victims pay through the same structure that protects owners, and the enforcement machinery exists to hold that asymmetry against sustained resistance. Tangled rope holds both halves. On mandatrophy: the founding problem — standing armies and federal disarmament — is contested rather than dead, because the modern arrangement is invoked almost entirely against regulation rather than against disarmament, while the self-defense need it also protects remains live. founding_problem_status is therefore contested, the R5 mismatch flag (dead plus world_rearranges) does not fire, and mandatrophy is not declared resolved: the arrangement's persistence is contested-purpose persistence, not zombie persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the second_amendment_boundary kernel: what structurally changes if a sibling reading (militia_conditioned_reading, insurrectionist_reading) is instantiated instead?',
    'Comparative authoring of the sibling stories. The militia-conditioned reading moves private possession out of the protected domain and shrinks the victim set to harms outside militia-relevant context; the insurrectionist reading adds a resistance-capacity function and redraws the beneficiary set. The disagreement is located in a single structural element: whether the prefatory clause limits the operative clause''s scope.',
    'The victim set, the coordination function, and the enforcement structure all change with the reading; epsilon and classification are reading-indexed and not comparable across readings without normalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed structure of a contested constitutional kernel.').

omega_variable(
    victim_set_reading_contestation,
    'Does this reading''s own seat accept that the counted harms (mass-casualty events, domestic homicide, firearm suicide) are costs the boundary imposes, or does it hold them to be the price of a legitimate pre-existing liberty outside the cost ledger?',
    'Seat-level comparison: the owner, industry, and enforcing-judiciary seats compute the boundary as right-protection with near-zero imposed cost; the victim seats compute enforced imposition with trapped exit. The divergence is structural, not a measurement error.',
    'If the reading''s self-assessment governs, effective imposed cost drops toward the coordination floor and the type moves toward rope; if the victim-seat accounting governs, the current profile holds or hardens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_reading_contestation, conceptual, 'Reading-indexed status of the victim set.').

omega_variable(
    harms_causal_attribution,
    'What share of firearm mortality (suicide, domestic homicide, mass-casualty events) is causally attributable to the constitutional shielding itself, versus what would persist under any regime retaining an individual right of narrower scope?',
    'Cross-jurisdiction and cross-national comparison controlling for demographics, urbanization, and socioeconomic covariates; natural experiments from state-level policy variation and from the post-Bruen wave of struck and chilled regulations.',
    'A high attributable share supports the victim-set accounting and the current imposed-cost estimate; a low share recasts the boundary as coordination with incidental costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harms_causal_attribution, empirical, 'Causal share of firearm harm attributable to the constitutional shield.').

omega_variable(
    bruen_methodology_stability,
    'Will the historical-analogue test stabilize into a workable boundary, or collapse into judge-by-judge divergence that either hardens the foreclosure of regulation or dissolves it?',
    'Track the post-Bruen appellate record: circuit splits, en banc reversals, certiorari grants, and the rate at which regulations survive or fall.',
    'Collapse toward permissive divergence raises suppression and effective imposed cost (more regulation foreclosed); collapse toward restrictive divergence lowers both and moves the computed type toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bruen_methodology_stability, empirical, 'Stability of the enforcement methodology that sets the boundary''s effective width.').

omega_variable(
    defensive_use_genuineness,
    'How much of the beneficiary-side coordination claim (arms for lawful self-defense) rests on genuine defensive use versus contested survey inflation?',
    'Reconcile defensive-use survey estimates against incident-based data (national crime surveys, police records, justified-homicide counts); compute net benefit under both estimates.',
    'If genuine defensive use is a small fraction of the claimed benefit, the coordination function thins and the imposed-cost share of the structure rises; if substantial, the coordination component is firmer than the metrics assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_use_genuineness, empirical, 'Evidence basis for the beneficiary-side coordination claim.').

omega_variable(
    identity_lock_reversibility,
    'Is the fusion between firearm ownership and liberty/self-reliance self-concept strong enough that owner-side attachment to the boundary would persist even after a sustained doctrinal reversal?',
    'Longitudinal attitude and ownership data across jurisdictions adopting restrictive regimes; cultural persistence of ownership where constitutional protection is absent.',
    'If identity lock persists, the arrangement''s persistence is not enforcement-dependent and post-reversal classification shifts toward inertial maintenance; if it breaks, the boundary''s fate tracks the doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Durability of owner-side identity fusion under doctrinal change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(second_amendment_individual_right_tr_t0, second_amendment_boundary__individual_right_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(second_amendment_individual_right_tr_t0, observed).
narrative_ontology:measurement(second_amendment_individual_right_tr_t3, second_amendment_boundary__individual_right_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement_basis(second_amendment_individual_right_tr_t3, observed).
narrative_ontology:measurement(second_amendment_individual_right_tr_t6, second_amendment_boundary__individual_right_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(second_amendment_individual_right_tr_t6, observed).
narrative_ontology:measurement(second_amendment_individual_right_tr_t9, second_amendment_boundary__individual_right_reading, theater_ratio, 9, 0.24).
narrative_ontology:measurement_basis(second_amendment_individual_right_tr_t9, observed).
narrative_ontology:measurement(second_amendment_individual_right_tr_t12, second_amendment_boundary__individual_right_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(second_amendment_individual_right_tr_t12, observed).
narrative_ontology:measurement(second_amendment_individual_right_tr_t15, second_amendment_boundary__individual_right_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(second_amendment_individual_right_tr_t15, observed).
narrative_ontology:measurement(second_amendment_individual_right_tr_t18, second_amendment_boundary__individual_right_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement_basis(second_amendment_individual_right_tr_t18, observed).

% Extraction over time
narrative_ontology:measurement(second_amendment_individual_right_be_t0, second_amendment_boundary__individual_right_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(second_amendment_individual_right_be_t0, observed).
narrative_ontology:measurement(second_amendment_individual_right_be_t3, second_amendment_boundary__individual_right_reading, base_extractiveness, 3, 0.47).
narrative_ontology:measurement_basis(second_amendment_individual_right_be_t3, observed).
narrative_ontology:measurement(second_amendment_individual_right_be_t6, second_amendment_boundary__individual_right_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement_basis(second_amendment_individual_right_be_t6, observed).
narrative_ontology:measurement(second_amendment_individual_right_be_t9, second_amendment_boundary__individual_right_reading, base_extractiveness, 9, 0.55).
narrative_ontology:measurement_basis(second_amendment_individual_right_be_t9, observed).
narrative_ontology:measurement(second_amendment_individual_right_be_t12, second_amendment_boundary__individual_right_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(second_amendment_individual_right_be_t12, observed).
narrative_ontology:measurement(second_amendment_individual_right_be_t15, second_amendment_boundary__individual_right_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(second_amendment_individual_right_be_t15, observed).
narrative_ontology:measurement(second_amendment_individual_right_be_t18, second_amendment_boundary__individual_right_reading, base_extractiveness, 18, 0.65).
narrative_ontology:measurement_basis(second_amendment_individual_right_be_t18, observed).

% Suppression requirement over time
narrative_ontology:measurement(second_amendment_individual_right_su_t0, second_amendment_boundary__individual_right_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(second_amendment_individual_right_su_t0, observed).
narrative_ontology:measurement(second_amendment_individual_right_su_t3, second_amendment_boundary__individual_right_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement_basis(second_amendment_individual_right_su_t3, observed).
narrative_ontology:measurement(second_amendment_individual_right_su_t6, second_amendment_boundary__individual_right_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement_basis(second_amendment_individual_right_su_t6, observed).
narrative_ontology:measurement(second_amendment_individual_right_su_t9, second_amendment_boundary__individual_right_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement_basis(second_amendment_individual_right_su_t9, observed).
narrative_ontology:measurement(second_amendment_individual_right_su_t12, second_amendment_boundary__individual_right_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(second_amendment_individual_right_su_t12, observed).
narrative_ontology:measurement(second_amendment_individual_right_su_t15, second_amendment_boundary__individual_right_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(second_amendment_individual_right_su_t15, observed).
narrative_ontology:measurement(second_amendment_individual_right_su_t18, second_amendment_boundary__individual_right_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(second_amendment_individual_right_su_t18, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, insurrectionist_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the Second Amendment' covers one kernel read three ways; per the epsilon-invariance principle each reading instantiates a structurally distinct constraint with its own epsilon, beneficiary/victim structure, and classification, authored as its own file. This story is the individual_right_reading. The militia_conditioned_reading shares the referent arrangement but authors a different victim set (private-possession harms leave the protected domain's cost ledger) and a different coordination function (collective defense); the insurrectionist_reading adds a resistance-capacity function and redraws beneficiaries. The readings are linked through network.affects_constraints and typed through cs_structure.reading_relations rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
