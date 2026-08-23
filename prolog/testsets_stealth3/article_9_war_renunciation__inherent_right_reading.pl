% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 War-Renunciation Settlement — Inherent Right Reading
 *   domain: constitutional/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   Article 9 of Japan's post-war constitution contains two clauses:
 *   renunciation of war as a sovereign right, and non-maintenance of 'land,
 *   sea, and air forces, as well as other war potential.' The inherent-right
 *   reading, consolidated by government doctrine from the 1950s onward, holds
 *   that the first clause bars aggressive war while the second — read against
 *   the inherent right of self-defense recognized in international law —
 *   permits organizing 'minimum necessary' defensive capacity. On this
 *   reading the Self-Defense Forces are constitutionally legitimate but
 *   scope-bound, and the text operates as a proportionality threshold rather
 *   than a prohibition. This story authors that reading ALONE as a single
 *   ε-invariant constraint; the strict-pacifist and collective-self-defense
 *   readings are separate constraints in separate files, linked through the
 *   network block (sibling constraint_ids are inferred from this story's id
 *   pattern; flagged in dual_formulation_note). The ε referent is the
 *   standing arrangement under contest as administered: renunciation doctrine
 *   plus the scope-limited Forces plus the enforcement machinery — judicial
 *   deference, interpretive custody, procedural gating of revision — that
 *   holds it. Assessed by this reading's own lights, the settlement delivers
 *   what the reading endorses (no aggressive war, lawful defense) while
 *   generating real asymmetries: a concentrated base burden in Okinawa, a
 *   proportionality tax on defense planning, and suppression running in BOTH
 *   directions (against revision and against abolition alike). Measured
 *   extraction rises over the interval because practice has drifted past the
 *   'minimum necessary' mark since 2014, moving the arrangement away from
 *   what this reading itself endorses. KEY AGENTS (by structural
 *   relationship): - supreme_court_of_japan: agenda-setting judiciary
 *   (institutional/constrained) — validates the Forces while refusing full
 *   endorsement - cabinet_legislation_bureau: interpretive administrator
 *   (institutional/identity_locked) — authored and long policed the reading's
 *   boundaries - japan_self_defense_forces: primary beneficiary
 *   (organized/trapped) — collects organizational existence from the reading
 *   - us_alliance_planners: secondary beneficiary and partial payer
 *   (institutional/mobile) — receives basing and predictability, funds the
 *   deterrence gap - okinawan_base_host_communities: concentrated payer
 *   (moderate/constrained) — hosts the base burden -
 *   defense_expansion_advocates: payer with agenda leverage
 *   (powerful/constrained) — taxed by the proportionality threshold they are
 *   slowly dismantling - anti_militarist_activists: payer with ideological
 *   lock-in (organized/identity_locked) — the suppressed opposition seat -
 *   japanese_citizens_electorate: diffuse beneficiary-payer
 *   (organized/constrained) — consumes security, funds and legitimates the
 *   arrangement - china_south_korea_governments: excluded external voice
 *   (institutional/trapped) - constitutional_scholars: analytical observer
 *   (moderate/analytical)
 *
 * KEY AGENTS:
 *   - supreme_court_of_japan: agenda-setting judiciary (institutional/constrained) — upholds the Forces via deference doctrine
 *   - cabinet_legislation_bureau: interpretive administrator (institutional/identity_locked) — authored the reading, then lost custody of it in 2014
 *   - japan_self_defense_forces: primary beneficiary (organized/trapped) — organizational existence flows from the reading
 *   - us_alliance_planners: secondary beneficiary, partial payer (institutional/mobile) — basing access and a bounded ally; underwrites the deterrence gap
 *   - okinawan_base_host_communities: concentrated payer (moderate/constrained) — bears the settlement's most localized costs
 *   - defense_expansion_advocates: payer with agenda leverage (powerful/constrained) — pays the proportionality tax while eroding the threshold
 *   - anti_militarist_activists: payer with ideological lock-in (organized/identity_locked) — loses every decisive contest, cannot exit without dissolving their identity
 *   - japanese_citizens_electorate: diffuse beneficiary-payer (organized/constrained) — sustains the ambivalence the settlement runs on
 *   - china_south_korea_governments: excluded voice (institutional/trapped) — objects from outside the process entirely
 *   - constitutional_scholars: analytical observer (moderate/analytical) — documents the doctrine-practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.55).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.54).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 War-Renunciation Settlement — Inherent Right Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'ebfa1716-1402-4506-bf50-f27c689d0116').
narrative_ontology:cs_kernel_codification('ebfa1716-1402-4506-bf50-f27c689d0116', fixed_text).
narrative_ontology:cs_authority_grounding('ebfa1716-1402-4506-bf50-f27c689d0116', lineage).
narrative_ontology:cs_interpretation_layer_present('ebfa1716-1402-4506-bf50-f27c689d0116').
narrative_ontology:cs_reading_relation('ebfa1716-1402-4506-bf50-f27c689d0116', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('ebfa1716-1402-4506-bf50-f27c689d0116', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('ebfa1716-1402-4506-bf50-f27c689d0116', foundational, inherent_sovereign_self_defense_survives_renunciation).
narrative_ontology:cs_axiom_status(inherent_sovereign_self_defense_survives_renunciation, holdable).
narrative_ontology:cs_axiom_grounding('ebfa1716-1402-4506-bf50-f27c689d0116', inherent_sovereign_self_defense_survives_renunciation, deontological).
narrative_ontology:cs_axiom('ebfa1716-1402-4506-bf50-f27c689d0116', foundational, defensive_capacity_capped_at_minimum_necessary).
narrative_ontology:cs_axiom_status(defensive_capacity_capped_at_minimum_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ebfa1716-1402-4506-bf50-f27c689d0116', defensive_capacity_capped_at_minimum_necessary, instrumental).
narrative_ontology:cs_reference_frame('ebfa1716-1402-4506-bf50-f27c689d0116', inherent_right_minimum_defense_frame).
narrative_ontology:cs_drift_state('ebfa1716-1402-4506-bf50-f27c689d0116', contemporary_post_2015_security_laws, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ebfa1716-1402-4506-bf50-f27c689d0116', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japan_self_defense_forces).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, us_alliance_planners).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_citizens_electorate).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, okinawan_base_host_communities).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, defense_expansion_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, anti_militarist_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, us_alliance_planners).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, japanese_citizens_electorate).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, inherent_right_self_defense_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, yoshida_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, civilian_control_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides legal challenges to the Self-Defense Forces' constitutionality. In the Sunagawa line of cases it declined to dissolve the Forces, reasoning that whether an organization possesses war potential is a matter of high political discretion reserved to the cabinet and diet. Each ruling leaves the Forces standing while stopping short of endorsing any full account of Article 9. After decades of accumulated precedent, reversing course would require the court to repudiate its own holdings, so its practical options narrowed to continued deference.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, supreme_court_of_japan, agenda_setter,
    institutional, generational, constrained, national).

% Career lawyers in the cabinet office who author official interpretations of Article 9. From the 1950s until 2014 their opinions defined the permissible perimeter of the Forces, excluding collective defense, collective security, and overseas combat. In 2014 the cabinet adopted collective-self-defense doctrine without the bureau's prior sign-off, ending its gatekeeper role; the bureau retains staff functions without the veto it once exercised. Its professional authority was constituted by custody of the text's meaning, so the bypass struck at the basis of its institutional self-conception.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, cabinet_legislation_bureau, agenda_setter,
    institutional, biographical, identity_locked, national).

% Maintains ships, aircraft, and personnel under statutes framing every unit as self-defense. Procurement plans, deployment rules, and rules of engagement are drafted to fit the official reading; officers train for territorial defense and disaster relief while new missions arrive through reinterpretation rather than new text. The organization exists legally only inside this frame: dissolution or full remilitarization are decisions made entirely above it, and its leadership has consistently defended the frame that grants it existence.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japan_self_defense_forces, beneficiary,
    organized, generational, trapped, regional).

% Manage the mutual-security treaty from Washington and Yokota. The settlement gives them a treaty ally barred from independent warfighting, hosting forward installations on Japanese territory, and relying on American extended deterrence. They plan around Japanese capability ceilings, press incrementally for larger Japanese roles, and fund the resulting deterrence gap themselves. Posture adjustments, burden-sharing demands, and basing negotiations remain levers they control, and they can reweight commitments across the region if Japanese politics stall.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, us_alliance_planners, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, us_alliance_planners, payer).

% Live alongside roughly seven-tenths of the American installations in Japan, concentrated on well under one percent of national land. They endure aircraft noise, accident risk, crime incidents, and contingency-planning assumptions made elsewhere; land leases renew under expropriation procedure when owners refuse, and prefectural votes against new construction are overridden by national siting law. Moving to the main islands is physically possible but severs land tenure, family graves, and community continuity, so most stay and organize instead.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, okinawan_base_host_communities, payer,
    moderate, biographical, constrained, local).

% Factions inside the ruling party, the defense industry, and aligned research organizations seeking capabilities beyond the territorial minimum: strike missiles, carrier-capable refits, defense-industrial exports, eventually formal amendment. Every step requires reinterpretation campaigns, coalition maintenance, and public-opinion contests; they have moved the line steadily since 2014 but cannot leap to full revision, which requires two-thirds majorities in both houses plus a national referendum.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, defense_expansion_advocates, payer,
    powerful, biographical, constrained, national).

% Networks of litigants, unions, religious bodies, teachers, and the Article 9 Association who regard the Forces' existence itself as a breach of the constitution's language. They file suit, mount mass demonstrations, oppose revision electorally, and document each scope expansion. They have lost every decisive courtroom and referendum-stage contest to date, and their organizing identity is built on the constitutional commitment itself, so abandoning the fight would mean abandoning the group's reason for being.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, anti_militarist_activists, payer,
    organized, generational, identity_locked, national).

% Live under the settlement without ever voting on it directly: polling shows durable support for keeping the Forces running alongside durable opposition to rewriting the text, and majorities opposed the 2015 security legislation. They fund the defense budget through taxation, consume the security it purchases, staff the all-volunteer Forces, and would be the final deciders in any amendment referendum. Emigration exists as a theoretical exit and functions practically as none.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_citizens_electorate, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, japanese_citizens_electorate, payer).

% Neighboring states that experienced Japanese invasion and colonial rule. They observe each widening of the Forces' scope — patrol deployments, strike missiles, alliance integration — and lodge formal objections that tie Japanese military normalization to unresolved historical grievances. They hold no seat anywhere in Japan's constitutional process; their influence arrives only indirectly, as external pressure cited by domestic opponents of expansion, and geography denies them any exit from the consequences of Japanese rearmament.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, china_south_korea_governments, excluded,
    institutional, generational, trapped, continental).

% Academic jurists producing the interpretive literature that both camps draw on. Professional consensus long judged the official reading defective and the 2014 reinterpretation procedurally improper; state practice proceeded regardless. The persistent gap between scholarly judgment and government doctrine is itself part of the record they study, and their analyses supply the evidentiary backbone for opposition litigation and for any future amendment debate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__inherent_right_reading, us_alliance_planners).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__inherent_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, once for the whole nation, how security is provided after total defeat and occupation: war renounced as an instrument of policy, defense kept minimal and American-guaranteed, neighbors reassured, and domestic politics spared garrison-state competition. One settlement replaces recurring bargaining over rearmament at every crisis.
% TRANSFER_FUNCTION: Moves basing territory, noise, accident exposure, and wartime contingency burden from the nation at large onto Okinawan communities; moves fiscal resources from general revenue into a scope-limited defense establishment; moves interpretive authority over the founding text to cabinet lawyers and the courts; moves war decision-rights out of the state altogether under the renunciation clause while returning narrowly scoped defensive decision-rights.
% ABSENT_VOICES: Neighbor-state governments object to each scope expansion but stand wholly outside the constitutional process, entering only as pressure cited by domestic debaters. Okinawan representatives were excluded from the 1972 reversion terms that kept the bases in place. Anti-militarist litigants are heard in court and at the ballot box but have never once been decisive — their dissent is recorded and routinely overridden.
% DISAPPEARANCE_RATIONALE: Overnight disappearance forces immediate rearrangement: the Forces lose their legal basis and face dissolution or emergency reconstitution, the American alliance loses its host-nation footing, Okinawan land questions reopen under emergency law, regional balancing scrambles, and nearly eight decades of security expectations unwind within months.
% FOUNDING_PROBLEM: After the 1945 surrender, permanently disable the military establishment that had produced invasion and colonization across Asia, while leaving the disarmed nation survivable — accomplished by writing the renunciation of war into the constitution's own text.
% FOUNDING_PROBLEM_CORROBORATION: The renunciatory core is attested from outside any beneficiary set by the occupation-era record itself: the Potsdam Declaration's disarmament terms, Far Eastern Commission directives, and contemporaneous statements by the drafters (Shidehara's initiative, Ashida's insertion of clause two), plus independent scholarship tracing the drafting history. What is disputed — whether the founding problem remains live or has receded behind newer security problems — divides the seats along the lines recorded under absent_voices; no beneficiary-only attestation exists for the disputed half.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55 (rising through the series): the settlement extracts meaningfully — Okinawa's concentrated base burden, the proportionality tax on defense planning, and suppression of two opposed alternatives — but also delivers broad, real security value, so extraction sits mid-high rather than extreme. Suppression 0.54 is authored as a RAW structural property, unscaled by power or scope: it reflects the enforcement band that holds practice between revision and abolition, via judicial deference, procedural gating (supermajority-plus-referendum), and interpretive policing. Accessibility_collapse 0.35: alternatives do NOT fully collapse — the amendment path stays formally open and periodically attempted, and abolitionist litigation recurs — so the settlement resembles a construct that must be defended, not a law that merely is. Resistance 0.55: decades of litigation, the 1960 and 2015 mass movements, electoral opposition, and neighbor-state diplomacy. Theater 0.47: as practice diverged from text, a growing share of the settlement's activity became interpretive performance — annual Article 9 debates, ritual reaffirmations, post-hoc legal memoranda justifying decisions already taken — while the renunciatory core still does genuine work. Claim/metric independence: claimed_type tangled_rope is stated from structure (genuine coordination function PLUS asymmetric extraction PLUS active enforcement, all three canonically required and all three present); the metrics are authored independently as descriptive truths; the engine computes per-seat classifications from the structural data. The suppression series OSCILLATES: the 1960 spike (treaty-crisis enforcement against massive street resistance) is a side effect of external alliance politics, not an intermittent-reinforcement mechanism; enforcement then normalized through the 1980s-90s before hardening again after 2014. Base scalars are measured at the 2025 endpoint, i.e., the accumulation phase of that cycle. Coalition check: the payer seats HAVE combined — the 2015 coalition of Okinawan governors, student movements, scholars, and anti-militarist organizations came within reach of forcing a referendum — so the powerless/moderate payer seats carry latent coalition power that the organized atom understates. Identity-lock mechanisms: cabinet_legislation_bureau exhibits INSTITUTIONAL identity fusion (its authority was its interpretive custody, destroyed by the 2014 bypass); anti_militarist_activists exhibit IDEOLOGICAL identity fusion (the constitutional commitment constitutes the group); the Forces exhibit relational-institutional fusion (the frame that limits them is also the only frame in which they legally exist). If the CLB's custody frame broke, interpretive authority would migrate openly to political actors and the settlement's enforcement would become overtly majoritarian; if the pacifist identity frame broke, the principal resistance bloc would dissolve into ordinary interest-group politics.
 *
 * PERSPECTIVAL GAP:
 *   Inter-institutional divergence: the court experiences stewardship burden (each case risks the precedent stack it built); the bureau after 2014 experiences dispossession of a function it identified with; the political executive experiences the settlement as a lever to be incrementally reinterpreted. Same-level lateral divergence: defense_expansion_advocates (powerful) and anti_militarist_activists (organized) occupy the same national arena at adjacent power levels and experience OPPOSITE failures — the expansionists lose to procedure (they win policy but cannot touch the text), the abolitionists lose to outcome (they win arguments and lose every decision). Their differentiated exit options drive the divergence: expansionists are constrained by supermajority arithmetic, abolitionists are identity_locked. The engine computes these per-seat differences from the structural data; this story's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the Forces collect organizational existence; alliance planners collect basing access and strategic predictability (while paying the deterrence-gap cost — hence secondary_role payer, placing them near-symmetric, slightly beneficiary-side); the electorate collects security and anti-militarist reassurance while paying taxes (near-symmetric, mildly beneficiary-side). Victim declarations: Okinawan communities bear the settlement's most concentrated costs with no offsetting benefit stream; expansion advocates pay the proportionality tax on every desired capability; abolitionists bear the arrangement's existence itself. Suppression, again, is authored unscaled — only extractiveness is scaled by directionality and scope in the engine's arithmetic. Directionality override: okinawan_base_host_communities carries power_atom moderate, which the structural derivation would read as mid-band (protest capacity, elected governors). The true relationship sits nearer full-target (~0.80) because the seat receives no compensating benefit stream whatsoever and its exit is culturally prohibitive — the override corrects for concentrated-bearer status masked by nominal political capacity. The only OTHER moderate-power agent is the scholars' observer seat (exit analytical), which sits outside the extraction arithmetic, so the override's blast radius is contained. No other overrides were used: victim-plus-exit derivations suffice for the remaining seats (identity_locked already pulls abolitionists toward full-target; powerful-plus-victim already places expansion advocates high despite their agenda leverage).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — disabling the militarist machine — is CONTESTED, not dead: the renunciatory core demonstrably still binds (no war fought since 1945, no conscription, no declared offensive doctrine), so the mandate has not atrophied wholesale and this is not a piton. But the SCOPE-LIMITING sub-function is eroding as practice drifts past 'minimum necessary,' and theater rises in step as interpretive performance substitutes for textual governance. The tangled_rope classification prevents two mislabels: calling the whole settlement a snare ignores the genuine, load-bearing coordination (alliance stability, neighbor reassurance, domestic peace over rearmament); calling it a rope ignores the Okinawa asymmetry, the proportionality tax, and the bidirectional suppression. The mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges — no zombie flag fires, correctly. The drift watch runs through the theater series: if coordination hollows while performance continues (interpretive custody fully politicized, renunciatory norms reduced to ceremony), the settlement migrates toward piton; if the scope ceiling collapses entirely, it migrates into the collective reading's constraint with higher ε.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates only the inherent_right_reading of the article_9_war_renunciation kernel — would adoption of a sibling reading change this constraint''s structure?',
    'Adoption events are observable in the enforcing institutions: formal amendment, a Supreme Court holding on the Forces'' status, or cabinet-level doctrinal replacement. Track which reading the court, cabinet, and diet actually operate as scopes expand.',
    'Strict-pacifist adoption voids the Forces'' legality — the arrangement becomes an enforced fiction delivering the same payload through pure coercion (classification shifts sharply toward snare). Collective-adoption removes the scope ceiling this reading supplies — the proportionality threshold dissolves and measured extraction rises further. Either event terminates THIS story''s constraint and activates a different one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'One-of-three readings of a contested kernel; sibling adoption restructures this constraint rather than adjusting it.').

omega_variable(
    textual_anchor_dispute_location,
    'Where in the text does the sibling disagreement live — does clause one''s renunciation of ''war'' leave defensive organization permitted, or does clause two''s bar on MAINTAINING forces prohibit it categorically?',
    'Doctrinal analysis of the drafting record (including Ashida''s last-minute insertion of clause two) and of how enforcing institutions weight each clause when individual scopes expand.',
    'If clause two governs categorically, this reading''s permission structure collapses into the strict-pacifist constraint and the arrangement''s legitimacy rests on an interpretation its own text defeats; the ε authored here would belong to a different constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_anchor_dispute_location, conceptual, 'Locates the structural element (clause weighting) on which the readings diverge.').

omega_variable(
    minimum_necessary_threshold_content,
    'What force structure still satisfies ''minimum necessary'' for territorial defense — do counterstrike missiles, carrier-capable refits, and integrated missile defense exceed it?',
    'Threat-environment analysis against force-structure requirements: specify what a purely territorial defense of the archipelago requires, then compare procured and programmed capabilities against that baseline.',
    'If current procurement exceeds the threshold, practice has already migrated into the collective reading''s territory and this story''s ε understates present extraction; if not, the drift series overstates departure and the reference frame is intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_necessary_threshold_content, conceptual, 'Threshold content is jointly empirical (capability requirements) and preferential (acceptable threat exposure); resolution fixes whether this reading still describes the arrangement.').

omega_variable(
    okinawa_burden_attribution,
    'Is the concentrated base burden in Okinawa a cost of the Article 9 settlement, or of American force-posture choices that would persist under any Japanese constitutional reading?',
    'Comparative counterfactual and archival work: base distribution under a revised constitution or under the strict-pacifist reading; alliance negotiation records on why the footprint sits where it sits.',
    'If posture-driven, the Okinawan seat''s directionality toward THIS constraint drops and its victim attribution attenuates (shifting gain_flow reasoning); if settlement-driven — the reading is what makes the host-nation bargain necessary — victim attribution stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(okinawa_burden_attribution, empirical, 'Attribution of the settlement''s sharpest localized cost to the settlement itself versus to alliance posture.').

omega_variable(
    interpretive_custody_decay,
    'Can the settlement persist now that interpretive custodianship has passed from career lawyers (the bureau) to political cabinets?',
    'Track whether subsequent scope expansions proceed by legal-doctrinal argument or by bare political majority; monitor bureau staffing, opinion production, and whether courts reclaim the interpretive role.',
    'Politicized custody pushes theater_ratio higher and predicts drift toward piton — constitutional fidelity performed without the interpretive function that once made it operative; restored custody would flatten the theater series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_custody_decay, empirical, 'Persistence question about the enforcement machinery''s character after the 2014 bypass.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1947, 0.12).
narrative_ontology:measurement_basis(arti_tr_t1947, observed).
narrative_ontology:measurement(arti_tr_t1954, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1954, 0.17).
narrative_ontology:measurement_basis(arti_tr_t1954, observed).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(arti_tr_t1960, observed).
narrative_ontology:measurement(arti_tr_t1972, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1972, 0.27).
narrative_ontology:measurement_basis(arti_tr_t1972, observed).
narrative_ontology:measurement(arti_tr_t1981, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1981, 0.29).
narrative_ontology:measurement_basis(arti_tr_t1981, observed).
narrative_ontology:measurement(arti_tr_t1992, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1992, 0.33).
narrative_ontology:measurement_basis(arti_tr_t1992, observed).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2014, 0.43).
narrative_ontology:measurement_basis(arti_tr_t2014, observed).
narrative_ontology:measurement(arti_tr_t2025, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2025, 0.47).
narrative_ontology:measurement_basis(arti_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1947, 0.3).
narrative_ontology:measurement_basis(arti_be_t1947, observed).
narrative_ontology:measurement(arti_be_t1954, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1954, 0.34).
narrative_ontology:measurement_basis(arti_be_t1954, observed).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1960, 0.39).
narrative_ontology:measurement_basis(arti_be_t1960, observed).
narrative_ontology:measurement(arti_be_t1972, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1972, 0.43).
narrative_ontology:measurement_basis(arti_be_t1972, observed).
narrative_ontology:measurement(arti_be_t1981, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1981, 0.44).
narrative_ontology:measurement_basis(arti_be_t1981, observed).
narrative_ontology:measurement(arti_be_t1992, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1992, 0.47).
narrative_ontology:measurement_basis(arti_be_t1992, observed).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement_basis(arti_be_t2014, observed).
narrative_ontology:measurement(arti_be_t2025, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2025, 0.55).
narrative_ontology:measurement_basis(arti_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1947, 0.25).
narrative_ontology:measurement_basis(arti_su_t1947, observed).
narrative_ontology:measurement(arti_su_t1954, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1954, 0.33).
narrative_ontology:measurement_basis(arti_su_t1954, observed).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1960, 0.46).
narrative_ontology:measurement_basis(arti_su_t1960, observed).
narrative_ontology:measurement(arti_su_t1972, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1972, 0.42).
narrative_ontology:measurement_basis(arti_su_t1972, observed).
narrative_ontology:measurement(arti_su_t1981, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1981, 0.37).
narrative_ontology:measurement_basis(arti_su_t1981, observed).
narrative_ontology:measurement(arti_su_t1992, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement_basis(arti_su_t1992, observed).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2014, 0.49).
narrative_ontology:measurement_basis(arti_su_t2014, observed).
narrative_ontology:measurement(arti_su_t2025, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2025, 0.54).
narrative_ontology:measurement_basis(arti_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, resource_allocation).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the article_9_war_renunciation kernel per the ε-invariance principle: the colloquial label 'Article 9' covers three structurally distinct constraints — categorical prohibition (strict pacifism), threshold-on-defense (THIS story), and extended collective action (collective self-defense). Their ε values diverge because the readings disagree on what the arrangement IS: the pacifist reading assesses an illegitimate armed organization (maximal violation), this reading assesses a legitimate-but-drifting settlement (mid-band, rising), and the collective reading assesses a still-under-ceiling arrangement it wants to raise. Upstream/downstream: this reading is UPSTREAM of the collective reading (accepting an inherent right is the premise the 2014 reinterpretation built on — influences edge) and mutually exclusive with the strict-pacifist reading (forecloses edge). Sibling constraint_ids are inferred from this story's identifier pattern; if the sibling files minted different ids, the edges should be re-pointed to the actual filenames.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
