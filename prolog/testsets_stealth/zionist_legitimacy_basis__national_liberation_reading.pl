% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionist Legitimacy Basis — National Liberation Reading
 *   domain: political history/nationalism
 *
 * SUMMARY:
 *   This story authors the national-liberation instantiation of the Zionist
 *   legitimacy kernel as a standing arrangement: a stateless, persecuted
 *   people's claim to sovereign refuge in its ancestral homeland,
 *   institutionalized through international recognition (the Balfour
 *   Declaration, the Mandate, UN Resolution 181), realized in a state, and
 *   maintained since 1948 by that state's law and army. The ε referent is the
 *   standing arrangement — the Jewish national home as it exists, including
 *   the displaced Palestinian population and the occupied territories — and
 *   every value below is authored by this reading's own lights: persecution
 *   and historical connection justify the displacement; the arrangement's
 *   coercion is defensive; Arab opposition is classified as denial of Jewish
 *   rights rather than weighed as a competing claim. The claimed_type is the
 *   reading's own claim about what the arrangement is; the metrics are
 *   authored from the arrangement's operation as even this reading must
 *   concede it — founding displacement, permanent denial of return, military
 *   rule over a subject population — and the gap between claim and operation
 *   is left for the engine's per-seat computation, not reconciled here. This
 *   story is one reading of a contested kernel; see commentary.kernel_context
 *   and the routing omega for the sibling instantiations.
 *
 * KEY AGENTS:
 *   - persecuted_jewish_diaspora: primary beneficiary ([organized]/[constrained]) — the arrangement's refuge, recognition, and immigration channel flow to it; its exit from persecution ran through the arrangement
 *   - israeli_jewish_population: holding beneficiary ([organized]/[constrained]) — holds the state, land, and majority the arrangement delivers; partially identity-fused at its core
 *   - israeli_state_institutions: agenda-setter and co-beneficiary ([institutional]/[arbitrage]) — administers law, army, and the legitimacy narrative; collects the arrangement's revenues and territory
 *   - palestinian_displaced_population: primary payer ([powerless]/[trapped]) — bears the founding displacement and its standing denial of return
 *   - palestinians_under_military_rule: ongoing payer ([powerless]/[trapped]) — governed by the arrangement's military command without a vote in it
 *   - british_mandate_authority: historical co-architect ([institutional]/[arbitrage]) — issued Balfour, administered the Mandate, withdrew in 1948
 *   - great_power_guarantors: co-architect and shield ([institutional]/[arbitrage]) — recognition, aid, and veto cover; collect alliance value
 *   - arab_states_and_league: excluded objectors ([organized]/[mobile]) — rejected the settlement, absorbed the refugees, never accommodated within it
 *   - international_courts_and_historians: analytical observer ([analytical]/[analytical]) — adjudicates legality and reconstructs the archival record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.3).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.75).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionist Legitimacy Basis — National Liberation Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political history/nationalism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, 'e96ed968-eb08-4c0e-a89a-62b461edeebb').
narrative_ontology:cs_kernel_codification('e96ed968-eb08-4c0e-a89a-62b461edeebb', formalized).
narrative_ontology:cs_authority_grounding('e96ed968-eb08-4c0e-a89a-62b461edeebb', lineage).
narrative_ontology:cs_interpretation_layer_present('e96ed968-eb08-4c0e-a89a-62b461edeebb').
narrative_ontology:cs_reading_relation('e96ed968-eb08-4c0e-a89a-62b461edeebb', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('e96ed968-eb08-4c0e-a89a-62b461edeebb', zionist_legitimacy_basis__religious_restoration_reading, influences).
narrative_ontology:cs_axiom('e96ed968-eb08-4c0e-a89a-62b461edeebb', foundational, persecution_creates_sovereign_refuge_right).
narrative_ontology:cs_axiom_status(persecution_creates_sovereign_refuge_right, holdable).
narrative_ontology:cs_axiom_grounding('e96ed968-eb08-4c0e-a89a-62b461edeebb', persecution_creates_sovereign_refuge_right, empirically_contingent).
narrative_ontology:cs_axiom('e96ed968-eb08-4c0e-a89a-62b461edeebb', foundational, ancestral_return_grounds_legitimate_self_determination).
narrative_ontology:cs_axiom_status(ancestral_return_grounds_legitimate_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('e96ed968-eb08-4c0e-a89a-62b461edeebb', ancestral_return_grounds_legitimate_self_determination, deontological).
narrative_ontology:cs_reference_frame('e96ed968-eb08-4c0e-a89a-62b461edeebb', persecution_justified_ancestral_return).
narrative_ontology:cs_drift_state('e96ed968-eb08-4c0e-a89a-62b461edeebb', contemporary_occupation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e96ed968-eb08-4c0e-a89a-62b461edeebb', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, persecuted_jewish_diaspora).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_jewish_population).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, great_power_guarantors).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_displaced_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinians_under_military_rule).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_states_and_league).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, jewish_national_self_determination_principle).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, ancestral_return_legitimacy).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, refuge_for_persecuted_peoples_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stateless communities scattered across Europe, the Arab world, and later the Soviet bloc, facing expulsions, quotas, and massacres that no host state reliably prevented. The arrangement gives them a recognized national home: an immigration channel open to any Jew, eventual citizenship, and a state that claims the duty to receive them. Their route out of persecution ran through this arrangement; leaving it means returning to the vulnerability that preceded it. Individually powerless where they lived, collectively mobilized through national funds, congresses, and agencies.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, persecuted_jewish_diaspora, beneficiary,
    organized, generational, constrained, global).

% Holds what the arrangement delivers: a state, an army, a developed economy, and a Jewish majority between the river and the sea. Bears the arrangement's burdens too — conscription, wars, and the regional hostility the arrangement's history generated. Emigration is possible and real but stigmatized as desertion, and for a substantial core the state is constitutive of personal and family identity rather than a residence choice.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_jewish_population, beneficiary,
    organized, generational, constrained, national).

% Writes and enforces the laws that constitute the arrangement: the Law of Return, land administration, the military command in the occupied territories, and the education and public-diplomacy apparatus that maintains the legitimacy narrative. Sets the agenda for what the arrangement means and which changes to it are discussable; collects the revenues, territory, and standing the arrangement holds. Its exit is not from the arrangement but into reshaping it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions, beneficiary).

% Roughly seven hundred thousand people displaced in the 1947-49 war, and their descendants — now millions, registered as refugees across the region. Denied return by the arrangement's laws since 1948; stateless or holding precarious status in host countries. Their property, towns, and political standing were absorbed into the arrangement, and no seat in it represents them. The return door is closed and host-state integration was long formally withheld.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_displaced_population, payer,
    powerless, generational, trapped, regional).

% Several million people in the West Bank, East Jerusalem, and Gaza governed by the arrangement's military command rather than represented in it — living under permits, checkpoints, closures, and separate legal systems. Their movement, work, and family life are administered by the state whose founding arrangement displaced their kin. They cannot vote in the state that rules them and cannot exit the territory it controls.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinians_under_military_rule, payer,
    powerless, biographical, trapped, regional).

% Issued the Balfour Declaration and wrote the Mandate terms that made the Jewish national home an instrument of international law, then administered immigration and land policy under it for three decades — at rising cost, facing Arab revolt and Jewish insurgency — until it withdrew in 1948 and handed the question to the United Nations. Its strategic interest in the position was real; its costs in blood and treasury ended its tenure.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, british_mandate_authority, agenda_setter,
    institutional, biographical, arbitrage, regional).

% Voted the 1947 partition, recognized the state within minutes and years respectively, and have since supplied the military aid, diplomatic protection, and veto cover that hold the arrangement in place. They collect alliance value, intelligence cooperation, and a stable client relationship in a volatile region. Their recognition is itself part of the arrangement's legitimacy machinery; they could reshape its terms but have chosen to underwrite them.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, great_power_guarantors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, great_power_guarantors, agenda_setter).

% Rejected the partition settlement at its birth, invaded in 1948, absorbed the refugee population into camps and host cities, and fought the arrangement's state in successive wars. Their objection was voiced from the beginning and never accommodated within the settlement; they bear refugee-hosting costs and the political uses and burdens of the conflict. As sovereign states they can and sometimes do exit the confrontation — two have signed peace treaties.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states_and_league, excluded,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, arab_states_and_league, payer).

% Adjudicate the arrangement's legality — advisory opinions, occupation-law rulings, UN inquiries — and reconstruct its history from opened archives, including the displacement record that the arrangement's early narratives minimized. They hold no stake in the arrangement's survival; their findings feed legitimacy contests on all sides without deciding them.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_courts_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, israeli_jewish_population).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a dispersed, persecuted population into a single national collective with recognized legitimacy: common institutions, an immigration channel (aliyah), a recognized claim to a territory, and eventually a state with an army — solving the collective-action problem of a stateless people that no host society would fully absorb or protect.
% TRANSFER_FUNCTION: Moves land and sovereignty in Palestine from its Arab inhabitants to the Jewish national collective; moves the moral costs of that displacement onto a justification frame (persecution plus historical connection) that converts them into the price of liberation; moves diaspora capital, labor, and political allegiance into the national project; moves great-power recognition into the arrangement in exchange for alliance value.
% ABSENT_VOICES: The Palestinian Arab inhabitants. The Arab Higher Committee's rejection of partition was not accommodated; the displaced were excluded from the settlement that displaced them and denied return thereafter; Palestinians under military rule are governed by the arrangement without a vote in it. Their objection is voiced and documented but structurally outside the frame this reading authorizes — the frame classifies it in advance as denial of Jewish rights rather than weighing it as a competing claim.
% DISAPPEARANCE_RATIONALE: A state of more than nine million people, its army, its economy, its diaspora lifelines, and the regional order built around its existence all depend on the arrangement; overnight disappearance would strand citizens, unmake the refuge function that still operates, and trigger immediate regional rearrangement. The displaced population's claims would also rearrange — against a vacuum rather than a counterparty.
% FOUNDING_PROBLEM: European antisemitic persecution and Jewish statelessness: a people without sovereignty, carrying a historical memory of expulsion from the land, facing recurring massacres that culminated in the Holocaust — the problem was how a persecuted minority secures guaranteed refuge and collective self-defense.
% FOUNDING_PROBLEM_CORROBORATION: The persecution premise is corroborated from outside the benefiting parties by European state archives, Holocaust historiography, and contemporary antisemitism monitoring (OSCE and EU agency data). The claim that the problem remains live is corroborated by that same monitoring, but the Palestinian and Arab seats attest only that the problem existed while disputing that it licenses the arrangement's present form. No corroborating source outside the beneficiary set attests that the problem licenses the current arrangement — they attest the problem, not the justification.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).
:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.30 by this reading's lights: the reading holds the founding displacement and the standing denial of return justified by the persecution premise and the ancestral connection, so the illegitimate component it concedes is moderate rather than high — but not negligible, because the reading does not deny that the displacement happened or that millions live outside the arrangement's rights. Suppression is authored at 0.75 as a raw structural property, unscaled by power or scope: the arrangement demonstrably requires an army, a military occupation, emergency regulations, and a closed return door; the reading classifies this coercion as defensive, but classification does not reduce the coercion, and only extractiveness is scaled in the engine's computation. Theater ratio 0.36: the arrangement's core functions (immigration, state, defense) are real and load-bearing, but a large and growing share of its activity is legitimacy maintenance — public diplomacy, commemoration, advocacy against delegitimization campaigns — whose share rises as the justification burden grows. Accessibility collapse 0.55: within the reading's frame, alternatives (refugee return, bi-national equalization) collapse into 'denial of Jewish rights' and become unspeakable; outside the frame they persist as live positions, so the collapse is partial. Resistance 0.75: the arrangement has met sustained organized resistance for a century — revolt, war, intifada, boycott, legal campaign — and exists in permanent justified mobilization. The measurement series run on one shared grid (t = 0, 25, 50, 75, 100, 125 of an 1897–2022 interval; years 1897, 1922, 1947, 1972, 1997, 2022) with all three metrics authored at every point. The extractiveness series is deliberately non-monotonic — it peaks at the founding displacement (t=50) and settles as the reading's justification frame consolidates — while suppression_requirement rises monotonically, tracing the enforcement ratchet from movement-era persuasion through war to entrenched occupation. The claimed_type (rope) is the reading's own claim about what the arrangement is; the metrics and the structural declarations (victims, enforcement) describe its operation; the divergence is the datum, not an error to be tuned away.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the beneficiary seats the arrangement is the thing standing between them and the next expulsion: its costs are either invisible or classified as consequences of others' choices, and the persecuted-diaspora seat experiences the arrangement as pure lifeline. From the payer seats the same structure is the thing that emptied their towns in 1948, holds their return closed for three generations, and rules their kin by military decree without a vote — they experience no coordination benefit at any price. The agenda-setter seat (the state) experiences the arrangement as its own body: it cannot evaluate the structure from outside because it is the structure's administrator, author, and chief collector. Same-level asymmetry: the arab_states seat and the israeli_jewish_population seat hold comparable organized power, but the Arab states hold mobile sovereign exit (two have signed treaties and left the confrontation) while the Israeli Jewish population's exit is constrained by identity fusion — for a substantial core, leaving is not relocation but the dissolution of the national project they constitute, so the constraint's hold on that seat is identity-locked even where the legal exit exists. The reading's own seat is internally split in a way the scalar ε cannot show: it concedes the displacement as fact while holding it justified, which is why its authored extractiveness (0.30) sits far below what the payer seats experience and far above denial (0.0). The engine computes these divergences per seat; the story authors the structure and does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real flows: persecuted_jewish_diaspora and israeli_jewish_population receive refuge, sovereignty, and standing; israeli_state_institutions administers and collects (role agenda_setter with beneficiary secondary — it is both the arrangement's author and its principal collector, which is why it also appears in the beneficiaries array); great_power_guarantors collect alliance value and supply the recognition machinery. The victim declarations map to the founding displacement (palestinian_displaced_population) and its standing maintenance (palestinians_under_military_rule) — both trapped, both powerless, both near the full-target end. The excluded seat (arab_states_and_league) is deliberately not declared in the beneficiary or victim arrays: its relationship to the arrangement is adversarial and cost-bearing rather than a flow the arrangement runs through it, and declaring it would misread an external opponent as an internal payer. No directionality overrides are used: every seat with structural declarations derives correctly from beneficiary/victim data plus power and exit, and the undeclared seats' power-atom fallbacks (near-symmetric for the mandate and observer seats) are acceptable approximations documented here rather than corrected — an override keyed only by power atom would distort the institutional seats that derive correctly. Gain receipt: the arrangement's gains — land, sovereignty, standing — demonstrably accrue to the israeli_jewish_population seat, which gain_flow names; the state institutions administer those gains without being their final holder. Fixing cost: the seat that could fix the arrangement's standing costs (the state, with its guarantors) faces a fix — return, equalization — that its own legitimacy frame classifies as existential annihilation, so fixing is prohibitive relative to its benefit from the fixer's position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — persecuted statelessness — is live by this reading's lights and corroborated from outside the beneficiary set (state archives, Holocaust historiography, contemporary antisemitism monitoring), so this is not a zombie mandate: the arrangement still performs its founding function, and real persecuted populations still run through its immigration channel. The mandatrophy-relevant risk therefore runs in the less familiar direction: a live founding problem shielding a drifted practice. The liberation narrative was built for a movement that would liberate a people and complete itself; what persists is a state permanently governing a population it displaced — practice drift, not mandate exhaustion. The founding problem's liveness is real, and it is also the arrangement's best shield against revision. The classification keeps the two failure modes distinct. Reading the arrangement as pure extraction would erase the genuine refuge function that stateless persecuted people still rely on — the coordination half is real, and the reading's claim is not fabricated. Accepting the reading's claim at face value would erase the payer seats whose costs the justification frame converts into price — the extraction half is real, and the victims are not rhetorical. The per-seat computation holds both halves without letting either seat's experience stand in for the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the kernel zionist_legitimacy_basis (reading: national_liberation_reading). What would each sibling reading change structurally if instantiated over the same referent, and where exactly is the disagreement located?',
    'The sibling constraint stories themselves: the settler_colonial_reading authors the same referent with the displacement as colonial extraction (far higher ε, victims foregrounded, likely tangled_rope or snare at most seats); the religious_restoration_reading authors divine-promise grounding (theological axioms, different beneficiary structure). The disagreement is located in the justification premise — whether persecution plus ancestral connection offsets the displacement costs — not in the empirical record both readings accept. Cross-reading comparison is valid only across the kernel family, never within one story.',
    'If the settler-colonial sibling''s structural data is accepted, this reading''s low ε is exposed as frame-relative rather than fact-responsive and the arrangement''s classification shifts toward tangled_rope or snare at most seats; if this reading''s data is accepted, the sibling''s high ε is exposed as denying the persecution justification. Neither outcome is decidable inside this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Routing omega: one reading of a three-reading kernel; sibling readings are separate constraints over a shared referent.').

omega_variable(
    justification_offset_frame_dependence,
    'Is the persecution justification''s offset of displacement costs a fact-responsive quantity (varying with the severity of persecution, the war''s conduct, the availability of alternatives such as the rejected Uganda scheme) or constitutively frame-relative (whoever holds the justification premise sets the offset)?',
    'Comparative analysis across the kernel family plus counterfactual history: if the offset tracks documented facts (persecution''s lethality, the 1947-49 war''s conduct, the availability and rejection of territorial alternatives), it is fact-responsive; if identical facts yield opposite offsets under different rights-prioritizations, it is frame-relative.',
    'If frame-relative, ε for this constraint is reading-indexed all the way down and no seat-neutral classification of the arrangement exists — the corpus must carry the kernel family as the unit of analysis. If fact-responsive, the offset can be contested on evidence and this reading''s ε is falsifiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justification_offset_frame_dependence, conceptual, 'Whether the persecution justification that lowers this reading''s ε is evidence-sensitive or purely perspectival.').

omega_variable(
    return_denial_separability,
    'Is the denial of refugee return separable from the arrangement''s refuge function, or constitutive of it?',
    'Negotiated episodes and incremental channels: the Oslo-era return discussions, the 2000 Camp David parameter debates, family-reunification increments — if the refuge function (immigration, state, defense) operates while return channels open incrementally, the functions are separable; if every opening is treated as existential, they are constitutively linked.',
    'If separable, the return denial is a distinct cost layer this reading''s justification does not automatically cover and the reading''s ε is understated; if constitutive, part of the measured cost is the price of the refuge function itself and the reading''s ε holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(return_denial_separability, empirical, 'Whether the arrangement''s liberation function requires the return denial or merely coexists with it.').

omega_variable(
    indigenous_classification_framing,
    'Does the historical record (continuous Jewish presence in the land, the demographic composition of 19th-century Palestine, the origins of the displaced population) decide the ''indigenous return'' classification, or does the classification turn on a framing rule (how long absence extinguishes indigeneity, whether return under imperial auspices counts as colonization) that evidence cannot settle?',
    'Archival demographic reconstruction is already extensive (Ottoman censuses, land registries, village records); the residual question is the framing rule — what weight continuous presence, imperial sponsorship, and long absence each carry — which is settled by normative argument, not additional data.',
    'If the classification is frame-relative, this reading''s ''indigenous return'' premise and the settler-colonial sibling''s ''colonial arrival'' premise are both underdetermined by evidence and the kernel contest is conceptual at its root; if decidable, one sibling''s foundational premise fails on evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_classification_framing, conceptual, 'Whether ''indigenous return'' versus ''colonial arrival'' is evidence-decidable or constitutively framed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(zion_tr_t0, observed).
narrative_ontology:measurement(zion_tr_t25, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 25, 0.14).
narrative_ontology:measurement_basis(zion_tr_t25, observed).
narrative_ontology:measurement(zion_tr_t50, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(zion_tr_t50, observed).
narrative_ontology:measurement(zion_tr_t75, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement_basis(zion_tr_t75, observed).
narrative_ontology:measurement(zion_tr_t100, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 100, 0.33).
narrative_ontology:measurement_basis(zion_tr_t100, observed).
narrative_ontology:measurement(zion_tr_t125, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 125, 0.36).
narrative_ontology:measurement_basis(zion_tr_t125, observed).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(zion_be_t0, observed).
narrative_ontology:measurement(zion_be_t25, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement_basis(zion_be_t25, observed).
narrative_ontology:measurement(zion_be_t50, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement_basis(zion_be_t50, observed).
narrative_ontology:measurement(zion_be_t75, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 75, 0.42).
narrative_ontology:measurement_basis(zion_be_t75, observed).
narrative_ontology:measurement(zion_be_t100, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement_basis(zion_be_t100, observed).
narrative_ontology:measurement(zion_be_t125, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 125, 0.3).
narrative_ontology:measurement_basis(zion_be_t125, observed).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(zion_su_t0, observed).
narrative_ontology:measurement(zion_su_t25, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(zion_su_t25, observed).
narrative_ontology:measurement(zion_su_t50, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(zion_su_t50, observed).
narrative_ontology:measurement(zion_su_t75, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement_basis(zion_su_t75, observed).
narrative_ontology:measurement(zion_su_t100, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement_basis(zion_su_t100, observed).
narrative_ontology:measurement(zion_su_t125, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 125, 0.75).
narrative_ontology:measurement_basis(zion_su_t125, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Zionism's legitimacy' covers three structurally distinct claims that share one referent (the standing Zionist national-home arrangement) and diverge on ε: this story (national liberation — persecution and ancestral connection justify the displacement; ε authored low by the reading's lights), the settler-colonial sibling (the same displacement authored as colonial extraction; ε authored high), and the religious-restoration sibling (the arrangement authored as messianic fulfillment; theological grounding). Per the ε-invariance principle these are three constraints over one kernel, linked here. The upstream/downstream pattern runs national liberation → religious restoration (the state's existence is the religious reading's raw material), while the settler-colonial reading contests the national liberation reading's justification premise directly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
