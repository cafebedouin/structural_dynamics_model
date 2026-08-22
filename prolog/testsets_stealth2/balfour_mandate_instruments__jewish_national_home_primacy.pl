% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [SUPERSEDED]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Mandate for Palestine under the Jewish National Home Primacy Reading
 *   domain: international law/colonial administration/state formation
 *
 * SUMMARY:
 *   The Mandate instruments for Palestine (the Balfour Declaration as
 *   incorporated; the San Remo resolution 1920; the Mandate text confirmed
 *   1922, operative 1923) carry a dual textual structure: facilitation of
 *   Jewish immigration and close settlement on the land alongside a
 *   protection clause for the civil and religious rights of existing
 *   non-Jewish communities. This story instantiates the
 *   jewish_national_home_primacy reading of the balfour_mandate_instruments
 *   kernel: the facilitation clauses are the instruments' operative core and
 *   the protection clause is subordinate — the arrangement directs
 *   demographic and territorial transformation toward Jewish sovereignty,
 *   with the Jewish Agency holding quasi-governmental status under Article 4.
 *   The sibling readings
 *   (balfour_mandate_instruments__dual_obligation_indigenous_rights,
 *   balfour_mandate_instruments__mandatory_interpretive_discretion) are
 *   separate constraint files with their own epsilon, beneficiaries, and
 *   victims; this file does not hedge across readings or average over them.
 *   Per the kernel-reading epsilon referent rule, extractiveness (0.74) is
 *   authored for the standing arrangement under contest — the Mandate as
 *   actually administered under this reading, 1920-1947 — assessed by this
 *   reading's own lights, never for any arrangement this or another reading
 *   would prefer. The claimed_type and the metrics are independent authored
 *   facts: the claim states what this reading takes the structure to be; the
 *   metrics describe its observed operation, and any divergence between them
 *   is measurement, not error.
 *
 * KEY AGENTS:
 *   - british_mandatory_administration: Agenda-setter (institutional/constrained) — administers and enforces; collects strategic rents early, bears enforcement costs late, repudiates the reading in 1939
 *   - jewish_agency: Primary institutional beneficiary (organized/identity_locked) — Article 4 quasi-governmental status; land, immigration, and institutional machinery accrue here
 *   - zionist_institutions: Beneficiary (organized/identity_locked) — diaspora funding, settlement planning, Colonial Office leverage
 *   - jewish_migrants: Demographic beneficiary (moderate/identity_locked) — land, labor, and refuge; the transformation's human substrate
 *   - palestinian_arab_landholders: Primary target (moderate/constrained) — the land base transferred; every exit feeds the extraction
 *   - palestinian_arab_political_leadership: Target (organized/constrained) — representation structurally downgraded; leaders deported from 1937
 *   - displaced_arab_tenant_farmers: Target (powerless/trapped) — lose tenancy with compensation clauses enforced unevenly
 *   - absentee_arab_landlords: Secondary beneficiary (moderate/arbitrage) — capture sale premiums and exit the arrangement's costs entirely
 *   - exiled_arab_nationalist_leaders: Excluded voice (organized/trapped) — would veto the reading; physically removed by the enforcement machinery
 *   - league_permanent_mandates_commission: Analytical observer (analytical/analytical) — sees the dual language and records the divergence from practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.74).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.7).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.74).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Mandate for Palestine under the Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international law/colonial administration/state formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, 'c3131f4d-ae81-4704-95af-0df0b057f9cc').
narrative_ontology:cs_kernel_codification('c3131f4d-ae81-4704-95af-0df0b057f9cc', formalized).
narrative_ontology:cs_authority_grounding('c3131f4d-ae81-4704-95af-0df0b057f9cc', lineage).
narrative_ontology:cs_interpretation_layer_present('c3131f4d-ae81-4704-95af-0df0b057f9cc').
narrative_ontology:cs_reading_relation('c3131f4d-ae81-4704-95af-0df0b057f9cc', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('c3131f4d-ae81-4704-95af-0df0b057f9cc', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('c3131f4d-ae81-4704-95af-0df0b057f9cc', foundational, national_home_entitles_demographic_transformation).
narrative_ontology:cs_axiom_status(national_home_entitles_demographic_transformation, holdable).
narrative_ontology:cs_axiom_grounding('c3131f4d-ae81-4704-95af-0df0b057f9cc', national_home_entitles_demographic_transformation, instrumental).
narrative_ontology:cs_axiom('c3131f4d-ae81-4704-95af-0df0b057f9cc', secondary, jewish_agency_quasi_governmental_status).
narrative_ontology:cs_axiom_status(jewish_agency_quasi_governmental_status, holdable).
narrative_ontology:cs_axiom_grounding('c3131f4d-ae81-4704-95af-0df0b057f9cc', jewish_agency_quasi_governmental_status, conventional).
narrative_ontology:cs_reference_frame('c3131f4d-ae81-4704-95af-0df0b057f9cc', national_home_primary_purpose).
narrative_ontology:cs_drift_state('c3131f4d-ae81-4704-95af-0df0b057f9cc', white_paper_1939_repudiation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('c3131f4d-ae81-4704-95af-0df0b057f9cc', '2026-06-12T12:00:00Z').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, absentee_arab_landlords).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, displaced_arab_tenant_farmers).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, balfour_pledge_binding_obligation_doctrine).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, league_mandates_system_legality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Palestine under the Mandate terms as it interprets them: drafts the land ordinances, sets immigration schedules, appoints the High Commissioner, and maintains the security forces. Collects strategic value early (position on the Suez route, imperial standing, wartime bases) and pays escalating enforcement costs late as Arab resistance and then Zionist insurgency grow. Its exit is referral of the whole question to the United Nations, taken in 1947 after two decades in which leaving meant abandoning a strategic position it could not easily replace.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_administration, agenda_setter,
    institutional, generational, constrained, global).

% Reviews the mandatory's annual reports and petitions from both communities, questions British representatives in Geneva, and records where administration diverges from the instruments' dual language. It can admonish and publish but cannot compel; its findings feed a League Council that never overrules the administering power on Palestine.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_permanent_mandates_commission, observer,
    analytical, generational, analytical, global).

% The World Zionist Organization, its executive, and the Jewish National Fund hold recognized standing, channel diaspora funds into land purchase, plan settlement, and lobby the Colonial Office and the League. The movement's legitimacy and identity are constituted by fulfilling the national home pledge; abandoning the framework would dissolve its reason for existing rather than merely cost it position.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    organized, generational, identity_locked, global).

% Recognized under Article 4 as the public body advising and cooperating with the administration on matters affecting the national home. Runs immigration intake, settlement planning, and land acquisition through associated bodies, and organizes Jewish labor. It participates in governing without bearing the administration's security costs; its quasi-governmental status is the arrangement's institutional centerpiece and the seat where land, standing, and immigration capacity demonstrably accrue.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency, agenda_setter).

% Enter under immigration certificates issued through the Agency; receive land, credit, and labor placement from Zionist institutions. Pioneer arrivals are ideologically committed to the national project; the post-1933 wave are refugees for whom Palestine is frequently the only open door, fusing rescue with demographic transformation. Their gains — land, work, and a citizenship-in-formation — are the arrangement's intended product.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, identity_locked, national).

% Hold the land base the national home requires. Zionist demand multiplies prices, tempting sales; refusal invites economic isolation and, after 1936, security measures against those who resist. Those who retain land face a shrinking political share as the demographic balance shifts. Every exit available — selling — feeds the transformation; staying absorbs its costs.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    moderate, generational, constrained, national).

% Organizes opposition through the Arab Executive, local national committees, and after 1936 the Arab Higher Committee; petitions London and Geneva; calls the general strikes. No institutional channel exists through which its consent could bind the administration — the instruments give the Jewish Agency official standing and no equivalent Arab body. Its leaders are deported or exiled from 1937 onward, and participation in London conferences yields no veto.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    organized, generational, constrained, national).

% Cultivate land registered to absentee owners; when parcels sell, protection-of-tenants clauses promise reinstallation or compensation that the administration enforces unevenly — the Hope Simpson survey counts thousands of landless families and the follow-up French report finds reinstallation largely unimplemented. They lose tenancy, move to wage labor in towns or marginal hillside plots, and have no political mechanism to contest sales. Coalition power is thin: dispersed, rural, and unorganized.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, displaced_arab_tenant_farmers, payer,
    powerless, immediate, trapped, local).

% Own large parcels in the Jezreel Valley, Beisan, and the coastal plain under Ottoman title while residing in cities or neighboring countries. Zionist demand multiplies land prices several-fold; they sell at premiums far above pre-Mandate values and exit agriculture entirely, frequently relocating abroad. Their sales supply the transformation's territorial base while Arab leadership publicly condemns them as national betrayers — they capture the constraint's price premium and exit its costs.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, absentee_arab_landlords, beneficiary,
    moderate, biographical, arbitrage, regional).

% Deported after the 1936-39 revolt — the Mufti to Lebanon, others to the Seychelles and detention camps — they continue directing opposition from exile. They would reject the primacy reading's premises outright and veto its machinery, but they are physically outside the negotiating framework; their exclusion is maintained by the same security apparatus that administers the arrangement.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, exiled_arab_nationalist_leaders, excluded,
    organized, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single successor administration for territory exiting Ottoman rule: courts, land title registration, public health, roads, and immigration processing are built once, centrally. Under this reading the instruments additionally coordinate the national home's construction itself — scheduling immigration, assembling a functioning land market, and chartering the Agency — so that state-building inputs arrive in planned sequence rather than ad hoc.
% TRANSFER_FUNCTION: Moves land title from Arab holders (disproportionately absentee owners' parcels with tenants in place) to Jewish collective and individual ownership via purchase facilitated by land ordinances; moves immigration capacity and official institutional standing to Zionist bodies; moves fiscal priority and administrative attention toward the national home; moves political representation and land security away from the Arab population, which holds no equivalent institutional standing.
% ABSENT_VOICES: The Palestinian Arab majority's consent was never solicited: no Arab body holds standing in the instruments, the 1919 King-Crane Commission's findings against the national home policy were set aside, Arab delegations to the London conferences received no veto, and the leadership remaining after 1937 was in exile or detention. Neighboring Arab governments and pan-Arab congresses claimed standing and were excluded from Mandate governance entirely.
% DISAPPEARANCE_RATIONALE: If the primacy reading's machinery — facilitated land transfer, scheduled immigration, Agency standing — vanished overnight, land sales would halt and title would remain with Arab holders, the Yishuv's institutional supremacy would lapse into ordinary minority-association rights, and the territory's demographic and political trajectory would reorganize around the existing Arab majority: the outcome the 1939 White Paper attempted and the 1947 partition recommendation superseded.
% FOUNDING_PROBLEM: Disposition of a contested post-Ottoman territory: giving the 1917 pledge of a 'national home' operative force — land access, immigration facilitation, institutional development — while administering a mixed population, and anchoring British strategic position in the eastern Mediterranean.
% FOUNDING_PROBLEM_CORROBORATION: The Peel Commission (1937), a Royal Commission external to the Zionist beneficiary set, attests the founding problem was real but that pursuing the national home through Mandate machinery had produced an irreconcilable conflict between two national communities. The 1939 White Paper, issued by the administering power itself, formally repudiates this reading's premise that the instruments committed Palestine to a Jewish state. The King-Crane Commission (1919) attests the problem was contested from the outset, recording near-unanimous Arab rejection of the national home policy. No Arab party corroborates the founding problem as legitimate; Arab testimony uniformly disputes it.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the transfer machinery — facilitated land purchase, scheduled immigration, Agency institutional supremacy — operated without any Arab consent mechanism: no Arab body held standing equivalent to the Agency, and the transfer was constitutive of this reading's purpose rather than an incidental cost. Suppression (0.70) is structural, not internalized: land ordinances, emergency regulations, deportation of political leaders, and collective punishment during the 1936-39 revolt held the arrangement against resistance by statute and force; the scalar is authored as a raw structural property, and the engine — not this story — scales extractiveness by directionality and scope. Theater (0.55) reflects the protection clause's trajectory: recited in annual reports and commissions through the 1920s with diminishing operative force, briefly revived by the Passfield White Paper and Hope Simpson protections (1930-31), then increasingly performative as the transformation proceeded. Accessibility collapse (0.55): alternatives were partly closed — the 1939 White Paper proved a different reading was administratively available and Arab sellers could opt in at a price — but no exit existed that preserved Arab land and political position simultaneously. Resistance (0.78) is among the highest for a Mandate-era arrangement: the 1920, 1921, and 1929 disturbances, the 1936-39 general revolt with its general strike and guerrilla campaign, and sustained petitioning in Geneva and London. The suppression_requirement series is authored because enforcement capacity is this story's dynamic: it ratcheted up through the revolt (peak 0.80 at 1939), decayed during the war (0.60 at 1943), and was redirected after 1945 against the primacy reading itself — immigration restrictions enforced against the Yishuv — which is why the arrangement's extractiveness bottoms in the war years and then tears upward to 0.74 at 1947 as the framework collapses into open conflict. The two dips in the extractiveness series (1931, 1939) are the two moments the administering power attempted to convert the arrangement toward the dual-obligation reading; both reversions were partial and temporary. All series share one time grid; every tracked metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same instruments. The payer seats (landholders, political leadership, tenant farmers) face a structure operating as extraction with suppressed exits: selling feeds the transformation, staying absorbs it, and no institutional channel converts opposition into veto. The beneficiary seats (Agency, Zionist institutions, migrants) face a coordination structure they built, staffed, and funded — from inside, the arrangement is the pledge being kept. The agenda-setter seat (Britain) computes a hybrid it progressively could not hold: strategic beneficiary early, enforcement-cost bearer late, explicit repudiator by 1939 — the White Paper is the administering power's own seat-classification flipping. Identity-lock dynamics: the Zionist seats are identity_locked because the movement's legitimacy was constituted by fulfilling the pledge, making exit unthinkable rather than merely costly; had that frame broken (a territorialist pivot accepted), the beneficiary seats would hold arbitrage-grade exit and the enforcement burden would have shifted entirely onto Britain. Same-power differentiation: Arab landholders and absentee Arab landlords hold the same moderate power atom, but the landlords' arbitrage exit splits them into a beneficiary seat — one structure pays one and extracts from the other, differentiated purely by exit option.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions, the Jewish Agency, and Jewish migrants are declared beneficiaries: the constraint subsidizes them with land access, immigration capacity, and official standing, so their derived directionality sits near the beneficiary end. Absentee Arab landlords are declared a separate beneficiary set with arbitrage-grade exit: they captured the sale premiums the transformation created and exited its costs entirely — declaring them separately from the landholder victim seat keeps the derivation honest, because a uniform Palestinian-Arab victim declaration would misderive their d. Palestinian Arab landholders and political leadership are declared victims with constrained exit: high d, amplified because every available exit (sale) feeds the extraction. Displaced tenant farmers carry the highest d: powerless, trapped, losing tenancy while compensation clauses go unevenly enforced. Britain carries no beneficiary or victim declaration: its net position is genuinely near-symmetric — strategic rents and imperial standing early, escalating enforcement costs and casualties late, formal repudiation at the end — so the canonical fallback for the institutional power atom (d near 0.5) is accurate and no directionality override is authored. The League's Permanent Mandates Commission is an analytical seat and feeds no extraction arithmetic. Scope amplification is modest here: the arrangement operates at national territorial scale with imperial oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving the national home pledge operative force while administering a mixed population — was progressively achieved rather than outlived: by interval end the Yishuv held state-like institutions, contiguous land, and armed capability, and the arrangement dissolved into the state it had been building. The classification guards against two mislabelings. Pure snare would erase the real coordination the Mandate performed (courts, Torrens-style title registration, public health and works that served all residents on paper); pure rope would erase the constitutive asymmetry — the coordination was built to transform demography and territory, and the transformation was the extraction. Tangled rope holds both, which is why requires_active_enforcement is true: nothing in the arrangement was self-sustaining by consent. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): the parties dispute whether the founding problem was ever legitimate while the world demonstrably rearranges around the arrangement's operation. This is not a zombie profile — the function did not atrophy; it completed — but the mandate ended in repudiation and dissolution rather than sunset. The 1939 White Paper was a mid-flight mandate conversion attempt that failed because the facts the arrangement had created (title, institutions, armed settlement) cost more to unwind than to persist with: the tangled_rope signature of extraction that has become the terrain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the jewish_national_home_primacy reading of the balfour_mandate_instruments kernel; would the dual_obligation_indigenous_rights sibling change the constraint''s beneficiary/victim structure and epsilon?',
    'Comparative drafting-history analysis (Balfour Declaration correspondence, Curzon''s 1919 objections, League Covenant Article 22 and the Mandate debates in Geneva) to determine whether the instruments'' text and drafting history commit to primacy or to equal-or-superior protection obligations.',
    'Under the dual-obligation reading, the same instruments compute with Arab protection as the coordination function and the national home as the constrained activity: beneficiaries and victims swap positions, epsilon falls substantially, and the classification moves toward rope-with-enforcement-violations rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Which reading of the kernel the instruments'' text and drafting history actually commit to.').

omega_variable(
    mandatory_discretion_boundary,
    'Is the operative constraint the primacy reading''s content, or the mandatory power''s discretionary adjudication between readings (the mandatory_interpretive_discretion sibling)?',
    'Trace whether specific extraction outcomes (land ordinances, immigration schedules, Agency recognition) derive from instrument text, from Colonial Office policy, or from High Commissioner discretion; counterfactual test: a different mandatory power administering the identical text.',
    'If discretion is the constraint, extraction is attributable to British administrative choice and the instruments are a permissive shell; the tangled_rope classification shifts toward the discretion sibling''s structure with Britain as the extraction''s seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatory_discretion_boundary, conceptual, 'Whether the extraction lives in the reading''s content or in the adjudicating power''s discretion.').

omega_variable(
    land_transfer_voluntariness,
    'How much of the Arab-to-Jewish land transfer was voluntary market exchange at voluntarily accepted prices, and how much was dispossession through tenant displacement, economic coercion, and distress sales?',
    'Land registry records, the Hope Simpson (1930) and Lewis French (1931) surveys of landless Arabs, compensation-payment records, and land-price series against regional benchmarks.',
    'If transfers were substantially voluntary, epsilon falls and the coordination framing strengthens; if dispossession dominated, the constraint trends toward snare territory with the land market as extraction cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_transfer_voluntariness, empirical, 'Voluntariness of the land transfer mechanism that carries the demographic transformation.').

omega_variable(
    refugee_rescue_offset,
    'Does the refugee-rescue function of facilitated immigration (especially 1933-1945, under Nazi persecution and closed Western doors) offset the extraction that the demographic transformation imposes on the Arab population?',
    'Counterfactual analysis of alternative destinations available per migrant cohort (Evian Conference outcomes, US quota practice, British White Paper limits) against actual admission; welfare comparison of admitted versus refused cohorts.',
    'If rescue was substantial and alternative destinations genuinely closed, part of the immigration facilitation is humanitarian coordination rather than transformation machinery, lowering effective epsilon; if alternatives existed, the facilitation is primarily demographic-transformation machinery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(refugee_rescue_offset, empirical, 'Whether immigration facilitation reads as rescue coordination or as extraction-enabling transformation machinery.').

omega_variable(
    founding_problem_legitimacy,
    'Was establishing a Jewish national home a legitimate Mandate objective at all, or was the founding problem itself the extraction — a pledge made by a power that did not hold the territory, over the recorded objections of its inhabitants?',
    'Not resolvable by data alone: depends on whether the pledge''s validity derives from the Allied war settlement''s conventional authority or requires the governed population''s consent under self-determination norms.',
    'If the founding problem is illegitimate, the constraint''s coordination function is void ab initio and the classification trends toward pure snare; if legitimate, the tangled_rope reading stands with extraction as the cost of an authorized project.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_legitimacy, preference, 'Legitimacy of the founding problem itself — the deepest framing dispute in the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1920, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bmnhp_tr_t1920, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1920, 0.25).
narrative_ontology:measurement_basis(bmnhp_tr_t1920, observed).
narrative_ontology:measurement(bmnhp_tr_t1925, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1925, 0.3).
narrative_ontology:measurement_basis(bmnhp_tr_t1925, observed).
narrative_ontology:measurement(bmnhp_tr_t1929, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1929, 0.38).
narrative_ontology:measurement_basis(bmnhp_tr_t1929, observed).
narrative_ontology:measurement(bmnhp_tr_t1931, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1931, 0.35).
narrative_ontology:measurement_basis(bmnhp_tr_t1931, observed).
narrative_ontology:measurement(bmnhp_tr_t1935, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1935, 0.45).
narrative_ontology:measurement_basis(bmnhp_tr_t1935, observed).
narrative_ontology:measurement(bmnhp_tr_t1936, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1936, 0.5).
narrative_ontology:measurement_basis(bmnhp_tr_t1936, observed).
narrative_ontology:measurement(bmnhp_tr_t1939, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1939, 0.4).
narrative_ontology:measurement_basis(bmnhp_tr_t1939, observed).
narrative_ontology:measurement(bmnhp_tr_t1943, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1943, 0.45).
narrative_ontology:measurement_basis(bmnhp_tr_t1943, observed).
narrative_ontology:measurement(bmnhp_tr_t1947, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1947, 0.55).
narrative_ontology:measurement_basis(bmnhp_tr_t1947, observed).

% Extraction over time
narrative_ontology:measurement(bmnhp_be_t1920, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement_basis(bmnhp_be_t1920, observed).
narrative_ontology:measurement(bmnhp_be_t1925, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1925, 0.55).
narrative_ontology:measurement_basis(bmnhp_be_t1925, observed).
narrative_ontology:measurement(bmnhp_be_t1929, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1929, 0.6).
narrative_ontology:measurement_basis(bmnhp_be_t1929, observed).
narrative_ontology:measurement(bmnhp_be_t1931, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1931, 0.58).
narrative_ontology:measurement_basis(bmnhp_be_t1931, observed).
narrative_ontology:measurement(bmnhp_be_t1935, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement_basis(bmnhp_be_t1935, observed).
narrative_ontology:measurement(bmnhp_be_t1936, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1936, 0.72).
narrative_ontology:measurement_basis(bmnhp_be_t1936, observed).
narrative_ontology:measurement(bmnhp_be_t1939, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1939, 0.66).
narrative_ontology:measurement_basis(bmnhp_be_t1939, observed).
narrative_ontology:measurement(bmnhp_be_t1943, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1943, 0.62).
narrative_ontology:measurement_basis(bmnhp_be_t1943, observed).
narrative_ontology:measurement(bmnhp_be_t1947, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1947, 0.74).
narrative_ontology:measurement_basis(bmnhp_be_t1947, observed).

% Suppression requirement over time
narrative_ontology:measurement(bmnhp_su_t1920, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1920, 0.35).
narrative_ontology:measurement_basis(bmnhp_su_t1920, observed).
narrative_ontology:measurement(bmnhp_su_t1925, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1925, 0.4).
narrative_ontology:measurement_basis(bmnhp_su_t1925, observed).
narrative_ontology:measurement(bmnhp_su_t1929, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1929, 0.5).
narrative_ontology:measurement_basis(bmnhp_su_t1929, observed).
narrative_ontology:measurement(bmnhp_su_t1931, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1931, 0.45).
narrative_ontology:measurement_basis(bmnhp_su_t1931, observed).
narrative_ontology:measurement(bmnhp_su_t1935, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1935, 0.55).
narrative_ontology:measurement_basis(bmnhp_su_t1935, observed).
narrative_ontology:measurement(bmnhp_su_t1936, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1936, 0.75).
narrative_ontology:measurement_basis(bmnhp_su_t1936, observed).
narrative_ontology:measurement(bmnhp_su_t1939, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1939, 0.8).
narrative_ontology:measurement_basis(bmnhp_su_t1939, observed).
narrative_ontology:measurement(bmnhp_su_t1943, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1943, 0.6).
narrative_ontology:measurement_basis(bmnhp_su_t1943, observed).
narrative_ontology:measurement(bmnhp_su_t1947, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1947, 0.7).
narrative_ontology:measurement_basis(bmnhp_su_t1947, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% The natural-language label 'the Mandate for Palestine' decomposes, per the epsilon-invariance principle, into at least three structurally distinct constraints — one per reading of the balfour_mandate_instruments kernel. This file instantiates the jewish_national_home_primacy reading (highest epsilon of the family: Zionist institutions and migrants as beneficiaries, Arab landholders and political leadership as victims, transformation as the operative directive). The dual_obligation_indigenous_rights sibling holds the protection obligations as governing and the national home as subordinated — its beneficiary/victim structure is inverted and its epsilon substantially lower. The mandatory_interpretive_discretion sibling locates the operative constraint in Britain's discretionary adjudication itself, attributing extraction to administrative choice rather than instrument text. The upstream claim (the pledge's binding status, vindicated by the Mandate's operation) is typically cited as warrant for this reading; the siblings contest that warrant. All family members link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
