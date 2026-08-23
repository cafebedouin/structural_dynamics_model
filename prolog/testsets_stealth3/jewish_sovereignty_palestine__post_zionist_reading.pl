% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Post-Zionist Reading: Ethnic-National Framework as Post-Attainment Obstruction
 *   domain: political/nationalism_studies/postcolonial
 *
 * SUMMARY:
 *   This story instantiates the post-Zionist reading of the kernel
 *   jewish_sovereignty_palestine: the Zionist project attained its object
 *   (sovereign statehood, 1948), but the ethnic-national institutional
 *   framework built for the attainment phase — the Law of Return's asymmetric
 *   admission, national-institution land development for Jewish settlement,
 *   the 2018 Nation-State entrenchment, and the occupation administration
 *   over non-citizen populations — now obstructs civic equality and regional
 *   integration. The epsilon referent is the standing arrangement as this
 *   reading sees it: the existing ethnic-national framework, never the
 *   de-Zionized civic state this reading endorses (which would score epsilon
 *   near zero by construction and is therefore worthless as a measurement).
 *   KEY AGENTS (by structural relationship): - israeli_state_institutions:
 *   agenda-setter (institutional/arbitrage) — writes and administers every
 *   rule in the stack, can rewrite any of them by ordinary legislation -
 *   jewish_citizens: primary beneficiary (organized/mobile) — collect
 *   admission rights, land access, constitutional recognition -
 *   diaspora_jews_with_aliyah_rights: secondary beneficiary
 *   (organized/arbitrage) — hold a standing membership guarantee exercisable
 *   at will - israeli_palestinian_citizens: target within the polity
 *   (moderate/constrained) — formal rights, differential rules -
 *   west_bank_palestinians and gaza_strip_residents: targets outside the
 *   polity (powerless/trapped) — governed without franchise -
 *   palestinian_refugees_diaspora: target across generations
 *   (moderate/constrained) — barred from the return granted freely to any Jew
 *   - international_human_rights_bodies: analytical observer
 *   (institutional/analytical). FAMILY NOTE: this is one member of a
 *   five-story constraint family; the sibling readings author sharply
 *   different epsilons over the same territory — liberal-nationalist low
 *   (self-determination coordination cost), settler-colonial very high
 *   (displacement regime regardless of citizenship), religious near-zero
 *   (divine fulfillment), cultural low (non-statist center). This reading
 *   authors moderate-high (0.71) because it locates the harm specifically in
 *   the post-attainment persistence of the ethnic framework, not in statehood
 *   as such.
 *
 * KEY AGENTS:
 *   - israeli_state_institutions: agenda-setter (institutional/arbitrage) — Knesset, ministries, courts, and military-civil administration; sets and enforces the entire rule stack
 *   - jewish_citizens: primary beneficiary (organized/mobile) — collect Law of Return admission, preferential land and budget access, exclusive constitutional self-determination clause
 *   - diaspora_jews_with_aliyah_rights: secondary beneficiary (organized/arbitrage) — hold unconditional membership rights they mostly do not exercise
 *   - israeli_palestinian_citizens: in-polity target (moderate/constrained) — enfranchised but governed under differential land, budget, and recognition rules
 *   - west_bank_palestinians: extra-polity target (powerless/trapped) — taxed and administered by a state whose elections they cannot vote in
 *   - gaza_strip_residents: extra-polity target (powerless/trapped) — perimeter-controlled without citizenship or pathway to it
 *   - palestinian_refugees_diaspora: intergenerational target (moderate/constrained) — return barred while identical return is automatic for Jews worldwide
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — investigate and report without domestic enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.71).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.68).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Post-Zionist Reading: Ethnic-National Framework as Post-Attainment Obstruction").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political/nationalism_studies/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, 'e35176c9-1389-45d7-ad57-6e3d2672d040').
narrative_ontology:cs_kernel_codification('e35176c9-1389-45d7-ad57-6e3d2672d040', formalized).
narrative_ontology:cs_authority_grounding('e35176c9-1389-45d7-ad57-6e3d2672d040', lineage).
narrative_ontology:cs_interpretation_layer_present('e35176c9-1389-45d7-ad57-6e3d2672d040').
narrative_ontology:cs_reading_relation('e35176c9-1389-45d7-ad57-6e3d2672d040', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e35176c9-1389-45d7-ad57-6e3d2672d040', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('e35176c9-1389-45d7-ad57-6e3d2672d040', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('e35176c9-1389-45d7-ad57-6e3d2672d040', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('e35176c9-1389-45d7-ad57-6e3d2672d040', foundational, ethnic_framework_mandate_exhausted_at_statehood).
narrative_ontology:cs_axiom_status(ethnic_framework_mandate_exhausted_at_statehood, holdable).
narrative_ontology:cs_axiom_grounding('e35176c9-1389-45d7-ad57-6e3d2672d040', ethnic_framework_mandate_exhausted_at_statehood, instrumental).
narrative_ontology:cs_axiom('e35176c9-1389-45d7-ad57-6e3d2672d040', foundational, civic_equality_requires_dezionized_institutions).
narrative_ontology:cs_axiom_status(civic_equality_requires_dezionized_institutions, holdable).
narrative_ontology:cs_axiom_grounding('e35176c9-1389-45d7-ad57-6e3d2672d040', civic_equality_requires_dezionized_institutions, deontological).
narrative_ontology:cs_reference_frame('e35176c9-1389-45d7-ad57-6e3d2672d040', post_attainment_civic_equality_frame).
narrative_ontology:cs_drift_state('e35176c9-1389-45d7-ad57-6e3d2672d040', basic_law_nation_state_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e35176c9-1389-45d7-ad57-6e3d2672d040', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jews_with_aliyah_rights).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, gaza_strip_residents).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Knesset, government ministries, courts, and the military-civil administration together write and operate every rule in the stack: who immigrates under the Law of Return, who leases state and development-authority land, how budgets distribute, how the territories are governed day to day. Any element can be rewritten by ordinary legislation; the choice to maintain rather than rewrite is made annually in budget and coalition cycles. Redesigning the constitutional identity itself is always formally available and politically ruinous for any sitting coalition.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).

% Constitute the state's dominant demos. Hold automatic admission for any Jew worldwide through the Law of Return, access to land developed by national institutions under terms unavailable to non-Jewish citizens, and a 2018 Basic Law declaring self-determination in the state uniquely theirs. Most experience the framework as ordinary life — school, army, mortgage, municipality — without encountering the sorting machinery that positions them favorably. Foreign passports are obtainable for many; attachment, service biography, and family keep nearly all in place.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens, beneficiary,
    organized, generational, mobile, national).

% Any Jew anywhere can arrive and receive citizenship on landing, with absorption packages, language instruction, and mortgage advantages attached. The overwhelming majority never exercise the option. The benefit is the standing guarantee itself: a refuge-and-membership right held in reserve, unmatched by any other group's relationship to any state, and invoked collectively in moments of perceived danger.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jews_with_aliyah_rights, beneficiary,
    organized, biographical, arbitrage, global).

% About a fifth of the citizenry: vote, sit in the Knesset, sue in the courts, staff hospitals and universities — and live under a lattice of differential rules. Villages admitted only by committee vetting, municipal budgets persistently below Jewish counterparts, land claims from 1948 frozen or extinguished, and a constitutional text that names their collective identity as outside the state's self-definition. Emigration is legal and physically open; leaving means leaving home, and virtually none treat it as an exit from the rules.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens, payer,
    moderate, generational, constrained, national).

% Live under military administration operated by the state: a permit regime governing movement, work, and building; jurisdiction partitioned so that most land and all border crossings answer to the administering power; home demolitions, detention, and settlement expansion on land they hold title to or farm. They pay fees and taxes into the administration and have no vote in the elections that determine its policies. Exit means abandoning land and livelihood; the wall and permit system close the daily routes out.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinians, excluded).

% Live behind a land-sea-air closure administered by the state in concert with Egypt: imports, exports, the fishing limit, electricity, and movement of people are all controlled from outside. Two million people hold no citizenship in the state that controls the perimeter, no pathway to acquiring one, and no functioning airport or seaport of their own. Exit is individually possible only through rare permits and Rafah crossings that open irregularly.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, gaza_strip_residents, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, gaza_strip_residents, excluded).

% Registered refugees across generations in Lebanon, Syria, Jordan, and beyond, with descendants numbering in the millions. The state's admission statute grants instant membership to any Jew on earth while barring their own return to homes within living memory. Advocacy runs through UNRWA, host-state politics where their status is itself contested, and transnational campaigns; none of these channels reaches the legislature that maintains the admission asymmetry.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_diaspora, payer,
    moderate, generational, constrained, global).

% UN treaty bodies, the International Court of Justice, and major human-rights organizations investigate the differential regime, publish findings, and occasionally refer matters to criminal tribunals. They commission legal analysis, take testimony from every seat, and hold no power to enact or repeal domestic legislation; their leverage runs through state relationships and public opinion.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state solves real coordination problems for everyone under its administration: security provision, infrastructure, municipal services, a common legal order, health and pension systems. The ethnic-national layer additionally coordinates Jewish collective membership — automatic worldwide admission, land development for Jewish settlement, national institutions serving Jewish collective aims — which is a genuine coordination function for that population, performed through the same apparatus that sorts everyone else.
% TRANSFER_FUNCTION: Moves land access, housing finance, budget allocations, immigration rights, and constitutional recognition toward Jewish citizens; moves permit burdens, demolition exposure, residency insecurity, and second-tier civic standing onto non-Jewish populations; reserves 'return' as an unconditional grant to any Jew worldwide while barring it categorically to Palestinian refugees.
% ABSENT_VOICES: West Bank and Gaza populations are governed by the state's military-civil apparatus with no vote in the polity that sets their rules — they would object to the entire arrangement and are present only as administered subjects. Palestinian refugees would object to the admission asymmetry and sit outside every forum that maintains it. Inside the polity, Palestinian citizens hold votes, but their equality program ('a state of all its citizens') is effectively excluded from coalition formation — present in the room, absent from the decision.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished overnight — Law of Return rescinded, land regimes opened, occupation administration dissolved, entrenchment clauses struck — the region's legal and demographic order would rearrange immediately: millions of refugees would press return claims, the settlement enterprise would lose its enabling apparatus, neighboring states would renegotiate relationships built on the current configuration, and the state's constitutional identity would require wholesale reconstruction. Nothing about the arrangement is self-maintaining; every element is actively administered.
% FOUNDING_PROBLEM: European Jewish statelessness: centuries of persecution capped by the Holocaust left Jews scattered, expropriable, and without a sovereign refuge or collective self-determination. Zionism was built to solve exactly that problem.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated by an overwhelming external record — Holocaust scholarship, European archives, contemporaneous diplomatic correspondence — that no party disputes. Its STATUS is what divides: Jewish communal security agencies and independent antisemitism-monitoring bodies (external to the state) attest that diaspora persecution risk remains material, supporting 'live'; Palestinian historians, UN reporting, and the Israeli 'New Historians' working from state archives attest that the problem was solved at statehood and that the framework now generates the harms this reading documents, supporting 'dead'. Corroboration exists on both sides from sources outside the beneficiary set; no external source settles the question, hence contested.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.71 (moderate-high, matching the reading's structural delta): the transfer surface is large and ethnically sorted — admission rights, land development, budget flows, constitutional recognition — but the state simultaneously delivers real services and security to all residents including its Palestinian citizens, which caps epsilon below snare range. Suppression 0.68 is a raw structural property, unscaled by power or scope: permit regimes, closures, demolitions, and residency-revocation instruments are legal facts, not perceptions. Theater ratio 0.36: a growing share of activity defends the democratic brand (equality proclamations, hasbara infrastructure, 'Jewish and democratic' reconciliation rhetoric) against the documented practice the framework itself produces. Accessibility collapse 0.55: alternatives (civic-equality legislation, binational confederation, refugee-return frameworks) remain fully articulable — they are not natural-law foreclosed — but are foreclosed within the current coalition structure; individuals' exits are separately collapsed (see stakeholder exit atoms). Resistance 0.70: sustained litigation (Adalah and successors), joint-list electoral politics, intifadas, boycott campaigns, ICC referrals, and mass protest waves. The temporal series run on ONE shared grid (t = 0, 15, 30, 45, 60, 75 years from 1948) with every tracked metric authored at every point. The series are non-monotonic by design: the dips at t=15 (end of military government over Arab citizens) and t=45 (Oslo-era relief) reflect genuine relaxations, and the recoveries reflect re-accumulation — episodic peace-process shocks modulating a persistent structure, not an oscillation that is itself the extraction mechanism. COORDINATION-TYPE GAMING ALERT: identity_coordination is declared because the framework genuinely maintains a membership boundary (this is its real function, not merely its cover story), but the Power x Scope coupling concentrates extraction on powerless agents at regional-to-global scope — exactly the pattern the offset must not excuse. COALITION NOTE: the four payer seats are structurally blocked from joint action — citizens, subjects, besieged residents, and diaspora refugees hold different legal statuses in different host polities — which is why resistance arrives serially and the constraint survives it.
 *
 * PERSPECTIVAL GAP:
 *   Four seats compute different types from one structure. From the agenda-setter seat the framework is the polity's constitutive achievement, maintained deliberately and defensible in its own terms. From the Jewish-beneficiary seats it is background normality — most beneficiaries never encounter the machinery that sorts them favorably. From the Palestinian-citizen seat it is lawful subordination: real votes, real courts, and a lattice of rules that place their collective identity outside the state's self-definition. From the occupied seats it is alien rule without franchise. Identity-lock dynamics: a subset of Jewish citizens are bound by ideological and institutional identity fusion (army-service biography, national-institution membership, existential-security worldview) that makes even available physical exit psychologically unthinkable; if that identity frame broke — for instance if external refuge guarantees were perceived as substitutable — their effective exit options would widen and their computed directionality would shift toward arbitrage, weakening the beneficiary coalition that maintains the framework. Suppression here is structural throughout (statutes, permits, force); no internalized-suppression omega is warranted, unlike interpersonal cases.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: jewish_citizens and diaspora_jews_with_aliyah_rights sit near the full-beneficiary end (d near 0.0) — the framework subsidizes them through admission rights, land access, and recognition, and their exit options (mobile, arbitrage) push them further toward subsidy. The four victim declarations drive the targets: israeli_palestinian_citizens (constrained exit) sit high; west_bank_palestinians and gaza_strip_residents (trapped, powerless, regional scope amplifying verification difficulty) sit nearest the full-target end; palestinian_refugees_diaspora (constrained, global scope) sit high with the return asymmetry as their specific extraction vector. The state institutions are the administrator, not the collector: receipt of the extraction concentrates in the jewish_citizens seat, recorded in gain_flow. No directionality overrides were needed — the beneficiary/victim declarations plus exit atoms produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Jewish statelessness with no sovereign refuge, capped by the Holocaust — was solved at attainment. The framework built to solve it persisted past its object and accreted new functions: occupation administration, settlement expansion, constitutional entrenchment. The mandatrophy-resolved condition is approached but not cleanly reached, because the refuge function retains live defenders (see the founding_problem_liveliness omega) and the coordination functions are real. Classification discipline cuts both ways: calling the arrangement a pure snare erases the service, security, and absorption coordination that any successor must rebuild and that Palestinian citizens demonstrably consume; calling it a pure rope erases the documented asymmetries the same structure distributes. Tangled_rope is the structurally honest claim: one apparatus, genuine coordination function, asymmetric extraction through the same apparatus, active enforcement required to hold the sorting in place. Theater_ratio tracks the performative share (equality rhetoric, brand defense) that grows as the gap between proclaimed and practiced civic equality widens — the classic proxy-drift signature of a mandate outliving its function while performing its original justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (post_zionist_reading) of the kernel jewish_sovereignty_palestine — how would each sibling reading restructure the victim set, beneficiary set, and epsilon if instantiated instead?',
    'Compare the compiled family: liberal_nationalist_reading authors low epsilon (coordination cost of legitimate self-determination, victims largely absent); settler_colonial_reading authors very high epsilon (displacement regime regardless of citizenship status); religious_zionist_reading authors near-zero epsilon (divine fulfillment admits no extraction concept); cultural_zionist_reading authors low epsilon with no sovereignty requirement. The disagreement is located in one structural element: whether the ethnic-national framework is a contingent instrument whose mandate ended at statehood (this reading) or constitutive of the polity''s legitimacy (liberal, religious) or of its injustice (settler-colonial).',
    'Classification is reading-indexed: the same territory, laws, and populations compute as rope-adjacent under the liberal reading, snare under the settler-colonial reading, and mountain-like under the religious reading. Cross-reading comparison is valid only at the family level, never by merging stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: this story is one indexed reading of a five-reading kernel; sibling readings are separate constraints, not components of this one.').

omega_variable(
    framework_amendability,
    'Is the ethnic-national framework a contingent policy stack amendable by ordinary legislation, or constitutive of the state''s identity such that civic-equality reform dissolves the polity?',
    'Constitutional analysis of the Basic Laws and Law of Return (are they ordinary statutes or entrenchment clauses?), comparative ethnocracy literature on whether comparable frameworks were reformed without regime dissolution, and the fate of ''state of all its citizens'' legislative proposals.',
    'If contingent, fixing_cost is legislative and the arrangement is a reformable tangled_rope; if constitutive, de-Zionization equals refoundation, fixing_cost is prohibitive in kind rather than degree, and naturalization rhetoric (''the only refuge'', ''existential necessity'') is doing constructed-constraint work that should be flagged rather than accepted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_amendability, conceptual, 'Whether the framework is legislated policy or constitutional essence — determines the cost class of repair.').

omega_variable(
    epsilon_scope_decomposition,
    'Does one epsilon correctly describe the framework across both the citizen body (where extraction is moderated by formal rights) and the occupied populations (where extraction is severe), or do the two zones measure as different constraints?',
    'Per the epsilon-invariance test: author a citizens-only variant and an occupation-focused variant. If the citizens-only measurement yields materially lower epsilon than the occupation-focused one, the label covers two structurally distinct arrangements sharing one legitimating narrative — decompose into a family (citizen-regime story, occupation-regime story) linked by network.affects_constraints, with this story as the umbrella reading.',
    'Decomposition would date type transitions separately: the citizen-side arrangement computes nearer rope/tangled_rope with lower suppression; the occupation-side arrangement computes nearer snare. The unified 0.71 blends them and may mask a snare-grade core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_scope_decomposition, conceptual, 'Whether the citizen-body zone and the occupied-zone operations share one stable epsilon or require family decomposition.').

omega_variable(
    coordination_extraction_separability,
    'Can the state''s genuine coordination functions (security, services, infrastructure, absorption) be separated from the ethnic allocation layer, or are they fused such that de-Zionization removes coordination capacity itself?',
    'Examine functioning sub-systems that already operate ethnically neutral (national insurance, health funds, universities'' civic tracks) versus fused systems (land development authorities, absorption ministry, admissions committees); assess proposals that preserve service delivery while opening membership allocation.',
    'If separable, the extraction layer is removable overhead riding on real coordination and the tangled_rope reading is stable; if fused, part of the measured extraction is the price of the coordination the state actually performs, and successor arrangements must budget for lost capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Structural separability of service coordination from ethnic privilege allocation.').

omega_variable(
    founding_problem_liveliness,
    'Does the founding problem (Jewish statelessness and persecution exposure) remain materially live, such that dismantling the preferential-admission framework would recreate the vulnerability it was built against?',
    'External antisemitism-incident data from monitoring bodies unaffiliated with either the state or its critics, weighed against the counterfactual risk profile of a civic-equality regime retaining asylum capacity without ethnic preference.',
    'If live, part of the framework''s persistence is load-bearing refuge provision and effective extraction should be discounted accordingly; if dead, the preference layer is retained purely by inertia and interest, strengthening the mandatrophy-resolved reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveliness, empirical, 'Empirical status of the founding problem''s recurrence risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jewi_tr_t15, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(jewi_tr_t45, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement(jewi_tr_t60, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(jewi_tr_t75, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 75, 0.36).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(jewi_be_t15, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(jewi_be_t45, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 45, 0.59).
narrative_ontology:measurement(jewi_be_t60, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(jewi_be_t75, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 75, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(jewi_su_t15, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(jewi_su_t45, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 45, 0.54).
narrative_ontology:measurement(jewi_su_t60, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(jewi_su_t75, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family for kernel jewish_sovereignty_palestine (five readings, five files). Decomposition follows the epsilon-invariance principle: the colloquial label 'Zionism/Israel' conflates structurally distinct claims whose epsilons differ by wide margins — the liberal-nationalist reading measures the coordination cost of a legitimate self-determination exercise (low epsilon); the settler-colonial reading measures a displacement regime that operates regardless of citizenship formalities (very high epsilon); the religious reading measures divine fulfillment admitting no extraction concept (near-zero); the cultural reading measures a non-statist spiritual-center project (low); this reading measures the post-attainment persistence of the ethnic-national framework (moderate-high, 0.71). The upstream/downstream structure: the liberal-nationalist reading is cited BY the state as legitimating cover for the framework this reading indicts, and the settler-colonial reading supplies evidentiary substrate this reading partially adopts while rejecting its totalizing scope. Every family member links to the others via affects_constraints; cross-reading comparison is valid only at family level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
